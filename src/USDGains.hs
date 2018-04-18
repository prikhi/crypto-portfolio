{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module USDGains
    ( State
    , initial
    , updatePrice
    , view
    ) where

import Brick
import Control.Concurrent (threadDelay)
import Control.Exception.Safe (catchIO)
import Control.Lens ((&), (^.), (.~))
import Control.Monad (unless)
import Data.Binary.Orphans (encodeFile, decodeFile)
import Data.List (partition, nubBy)
import Data.Maybe (fromMaybe, catMaybes, maybeToList)
import Data.Time (UTCTime)

import qualified Brick.Widgets.Border as B
import qualified Data.Map as Map

import GainsTable
import Types
import qualified Binance
import qualified GDAX
import qualified QuantityQueue as QQ


-- | Store the Data for the Gains table.
newtype State
    = State
        { gainsState :: GainsState
        }


-- | The Historical USD Prices for Currencies.
type PriceCache
    = Map.Map (Currency, UTCTime) Quantity


-- | Initialize the `PriceCache` & Calculate the Currency Totals.
--
-- Saves & loads a binary `usd-price-cache.bin` PriceCache file in the
-- current directory.
--
-- TODO: Pull cache file & location from XDG config, pass it in via a Env
-- type when we convert to `ReaderT Env IO`.
--
-- TODO: Can we generate this async-ly so there's not a huge wait during
-- startup?
-- maybe render w/ a progress bar or something(finished api reqs / total).
initial :: [Transaction] -> IO State
initial ts = do
    let transactions = reverse ts
    priceCache <- getPriceCache transactions
    return State
        { gainsState =
            GainsTable.initialGainsState (addTransaction priceCache) transactions
        }
    where
        priceCacheFilename :: String
        priceCacheFilename =
            "usd-price-cache.bin"
        getPriceCache :: [Transaction] -> IO PriceCache
        getPriceCache transactions = do
            initialCache <- loadCache
            priceCache <- buildPriceCache initialCache transactions
            encodeFile priceCacheFilename priceCache
            return priceCache
        loadCache :: IO PriceCache
        loadCache =
            catchIO (decodeFile priceCacheFilename) . const
                $ return Map.empty


-- | Build a Historical USD Price Cache for all Transactions.
--
-- Fetches the historical data from GDAX & Binance if it doesn't already
-- exist in the cache.
--
-- GDAX calls slow this down a lot - they only allow 3 per second.
buildPriceCache :: PriceCache -> [Transaction] -> IO PriceCache
buildPriceCache initialCache ts = do
    unless (null binanceTrades && null gdaxTrades) $
        putStrLn "Fetching Historical Prices. This may take a while."
    binancePrices <- concat <$> mapM getBinanceUSDPrice binanceTrades
    gdaxPrices <- catMaybes <$> mapM getGDAXPrice gdaxTrades
    return
        $ foldl (\m (k, v) -> Map.insert k v m) initialCache
        $ binancePrices ++ gdaxPrices
    where
        getGDAXPrice
            :: ((Currency, UTCTime), Maybe a)
            -> IO (Maybe ((Currency, UTCTime), Quantity))
        getGDAXPrice (c, _) =
            if Map.notMember c initialCache then do
                p <- uncurry GDAX.getHistoricalPrice c
                threadDelay 400000
                return $ Just p
            else
                return Nothing
        -- Get an Altcoin's USD price via it's Pair using Binance & GDAX.
        getBinanceUSDPrice
            :: ((Currency, UTCTime), Maybe Quantity)
            -> IO [((Currency, UTCTime), Quantity)]
        getBinanceUSDPrice (ct@(currency, time), maybeEthRatio) =
            if Map.notMember ct initialCache then do
                (pairCurrency, coinPrice) <- case maybeEthRatio of
                    Nothing ->
                        snd <$> Binance.getHistoricalPrice currency time
                    Just ratio ->
                        return (eth, ratio)
                pairPrice <- case Map.lookup (pairCurrency, time) initialCache of
                    Just q ->
                        return q
                    Nothing ->
                        threadDelay 400000
                            >> snd <$> GDAX.getHistoricalPrice pairCurrency time
                return
                    [ (ct, coinPrice * pairPrice)
                    , ((pairCurrency, time), pairPrice)
                    ]
            else
                return []
        gdaxTrades :: [((Currency, UTCTime), Maybe Quantity)]
        binanceTrades :: [((Currency, UTCTime), Maybe Quantity)]
        (gdaxTrades, binanceTrades) =
            partition ((`elem` GDAX.currencies) . fst . fst)
                $ filter (\(ct, _) -> Map.notMember ct initialCache)
                $ nubBy (\(a, _) (b, _) -> a == b) currenciesAndDates
        -- Build a list of historical prices to fetch, along with
        -- a potential altcoin/eth price.
        -- TODO: Add predicates like isFiat and isBinancePair so we can
        -- calculate btc/alt ratio, ltc/alt ratio, etc. instead of using
        -- historical binance prices or a case statement for each.
        currenciesAndDates :: [((Currency, UTCTime), Maybe Quantity)]
        currenciesAndDates =
            concat . flip map ts $ \t -> case transactionData t of
                Trade td ->
                    case (tradeBuyCurrency td, tradeSellCurrency td) of
                        -- Just use USD / Coin amount for price
                        (Currency "USD", _) ->
                            []
                        (_, Currency "USD") ->
                            []
                        -- Calculate & return the coin's ETH ratio.
                        (Currency "ETH", sCurrency) ->
                            ( (sCurrency, transactionDate t)
                            , Just $ tradeBuyQuantity td / tradeSellQuantity td
                            )
                            : map (withDate t)
                                (eth : maybeToList (tradeFeeCurrency td))
                        (bCurrency, Currency "ETH") ->
                            ( (bCurrency, transactionDate t)
                            , Just $ tradeSellQuantity td / tradeBuyQuantity td
                            )
                            : map (withDate t)
                                (eth : maybeToList (tradeFeeCurrency td))
                        -- Fetch the prices for both the Buy & Sell currencies
                        (bCurrency, sCurrency) ->
                            map (withDate t)
                                $ bCurrency
                                : sCurrency
                                : maybeToList (tradeFeeCurrency td)
                Transfer td ->
                    map (withDate t)
                        $ maybeToList
                        $ transferFeeCurrency td
                Income d ->
                    map (withDate t)
                        $ incomeCurrency d
                        : maybeToList (incomeFeeCurrency d)
                Expense ed ->
                    map (withDate t)
                        $ expenseCurrency ed
                        : maybeToList (expenseFeeCurrency ed)
        withDate :: Transaction -> Currency -> ((Currency, UTCTime), Maybe a)
        withDate t c =
            ((c, transactionDate t), Nothing)


-- | Modify the USD Currency Queues to Account for a new Transaction.
--
-- * Each Transaction type reduces any fee amount from the queues.
-- * Income adds to the queues with the historical value as the cost basis.
-- * Expenses reduce the queues & realize gains with the historical value
--   as the sale price.
-- * Trades increment & reduce their buys & sells respectively. The
--   historical prices are used for the buy's cost basis & sale's gains.
--
-- TODO: How to handle Expense fees? Should add the cost to the basis in
-- front of queue?
--
-- TODO: Sketch this out the flow on a piece of paper, make sure it makes
-- sense.
addTransaction :: PriceCache -> Transaction -> Queues -> Queues
addTransaction priceCache t = case transactionData t of
    Transfer td ->
        updateFeeCurrency (transferFeeCurrency td) (transferFeeQuantity td)
    Income d ->
        let
            marketValue =
                getHistoricalPrice "Income Market Value" $ incomeCurrency d
        in
            updateFeeCurrency (incomeFeeCurrency d) (incomeFeeQuantity d)
                . purchase (incomeQuantity d) marketValue (incomeFeeCurrency d)
                    (incomeFeeQuantity d) (incomeCurrency d)
    Expense ed ->
        updateFeeCurrency (expenseFeeCurrency ed) (expenseFeeQuantity ed)
            .  sell (expenseQuantity ed) (expenseCurrency ed)
    Trade td ->
        let
            buyPrice =
                getHistoricalPrice "Buy Price" $ tradeBuyCurrency td
        in
            updateFeeCurrency (tradeFeeCurrency td) (tradeFeeQuantity td)
                . updateSellCurrency td
                . updateBuyCurrency td buyPrice
    where
        purchase :: Quantity -> QQ.Price -> Maybe Currency -> Maybe Quantity -> Currency -> Queues -> Queues
        purchase q p maybeFeeCurrency maybeFeeAmount =
            updateQueues
                $ QQ.addPurchase q p
                $ feeTotal maybeFeeCurrency maybeFeeAmount
        sell :: Quantity -> Currency -> Queues -> Queues
        sell q c =
            updateQueues (QQ.addSale q $ getHistoricalPrice "Sell Market Value" c) c
        -- Update the queue for a Trade's buy Currency.
        updateBuyCurrency :: TradeData -> QQ.Price -> Queues -> Queues
        updateBuyCurrency td buyPrice =
            case (tradeBuyCurrency td, tradeSellCurrency td) of
                (Currency "USD", _) ->
                    id
                (bCurrency, Currency "USD") ->
                    let
                        buyPrice_ =
                            tradeSellQuantity td / tradeBuyQuantity td
                    in
                        purchase (tradeBuyQuantity td) buyPrice_
                            (tradeFeeCurrency td) (tradeFeeQuantity td)
                            bCurrency
                (bCurrency, _) ->
                    purchase (tradeBuyQuantity td) buyPrice
                        (tradeFeeCurrency td) (tradeFeeQuantity td)
                        bCurrency
        -- Update the queue for a Trade's sell Currency.
        updateSellCurrency :: TradeData -> Queues -> Queues
        updateSellCurrency td =
            sell (tradeSellQuantity td) (tradeSellCurrency td)
        updateFeeCurrency :: Maybe Currency -> Maybe Quantity -> Queues -> Queues
        updateFeeCurrency feeCurrency feeQuantity =
            case (,) <$> feeCurrency <*> feeQuantity of
                Just (currency, amount) ->
                    updateQueues (QQ.addFee amount) currency
                Nothing ->
                    id
        feeTotal :: Maybe Currency -> Maybe Quantity -> Maybe QQ.Price
        feeTotal maybeFeeCurrency maybeFeeAmount =
            case (,) <$> maybeFeeCurrency <*> maybeFeeAmount of
                Just (Currency "USD", amount) ->
                    Just amount
                Just (currency, amount) ->
                    Just $ amount * getHistoricalPrice "Fee Total" currency
                Nothing ->
                    Nothing
        updateQueues :: (QQ.Queue -> QQ.Queue) -> Currency -> Queues -> Queues
        updateQueues =
            updateCurrencyQueues (Currency "USD")
        getHistoricalPrice :: String -> Currency -> QQ.PricePerUnit
        getHistoricalPrice descr c =
            fromMaybe
                (error $ "USDG - " ++ descr ++ " Lookup Failure: " ++ show t)
                $ Map.lookup (c, transactionDate t) priceCache


-- | Update a Currency's data when it's Price changes. When Ethereum's
-- price changes, re-calculate the data for each alt coin.
updatePrice :: State -> Currency -> Quantity -> State
updatePrice (State s) currency quantity =
    let
        updatedCache =
            recalculateAllCurrencies
                $ updateUSDPrice (s ^. currencyCache) currency quantity
    in
        State $ s
            & currencyCache .~ updatedCache
            & aggregateData .~ calculateAggregates updatedCache
    where
        updateUSDPrice :: CurrencyCache -> Currency -> Quantity -> CurrencyCache
        updateUSDPrice c k q =
            if k `elem` GDAX.currencies then
                updateCachePrice c k q
            else
                getPrice (Binance.getPair k) c
                    & maybe c (updateCachePrice c k . (* q))
        -- Recalculate all ETH currencies when ETH's price changes.
        -- TODO: This should work with all possible GDAX pairs insted of just ETH.
        recalculateAllCurrencies :: CurrencyCache -> CurrencyCache
        recalculateAllCurrencies c =
            if currency == eth then
                case getPrice eth c of
                    Nothing ->
                        c
                    Just oldEthPrice ->
                        Map.foldlWithKey
                            (\c_ k _ -> recalculateUSDPrice c_ k oldEthPrice quantity)
                            c c
            else
                c
        -- If passed an altcoin with an ETH pairing, update the price using
        -- the change in ETH's price.
        recalculateUSDPrice :: CurrencyCache -> Currency -> Quantity -> Quantity -> CurrencyCache
        recalculateUSDPrice c k oldEthPrice newEthPrice =
            if k `elem` GDAX.currencies || Binance.getPair k /= eth then
                c
            else case getPrice k c of
                Just price ->
                    updateCachePrice c k $ price / oldEthPrice * newEthPrice
                Nothing ->
                    c


-- | Render the USD Gains Table.
view :: State -> [Widget AppWidget]
view (State s) =
    [ vBox
        [ B.hBorderWithLabel (str " USD Gains ")
        , B.border
            . padLeftRight 1
            . renderGainsTable USDGainsTable 2 [] s
            . Map.toList
            $ s ^. currencyCache
        ]
    ]
