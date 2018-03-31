{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module App where


import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent (ThreadId, forkIO, threadDelay, killThread)
import Control.Concurrent.STM (modifyTVar)
import Control.Monad ((<=<), void, forever, forM)
import Control.Monad.IO.Class (liftIO)
import Data.List (transpose, nub, nubBy)
import Data.Maybe (listToMaybe, fromMaybe, mapMaybe)
import GHC.Conc (TVar, STM, newTVar, readTVar, writeTVar, atomically)

import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Data.Map as Map
import qualified Graphics.Vty as V

import Binance
import CoinTracking
import GDAX
import Types


-- General Brick App Configuration & Initialization

-- | The Brick Configuration for the application.
config :: App AppState AppEvent AppWidget
config =
    App
        { appDraw = view
        , appChooseCursor = const listToMaybe
        , appHandleEvent = update
        , appStartEvent = liftIO . updateFromCaches
        , appAttrMap = styles
        }

-- | Create a Brick Event Channel that produces a `Tick` event 60 times per
-- second.
makeTickChannel :: IO (BChan AppEvent, ThreadId)
makeTickChannel = do
    channel <- newBChan 10
    threadId <- forkIO $ forever $ do
        writeBChan channel Tick
        threadDelay $ ticksPerSecond 60
    return (channel, threadId)
    where ticksPerSecond tps = 1000000 `div` tps



-- MODEL

-- | The state of the application, including TVars as caches & asynchronous
-- update channels.
data AppState
    = AppState
        { appTransactions :: [Transaction]
        , appCurrencyCache :: CurrencyCache
        , appAggregateData :: AggregateData
        , appCacheTVars :: (TVar [Transaction], TVar CurrencyCache, TVar PriceUpdateQueue)
        , appPriceThreads :: [ThreadId]
        }

-- | A List of Price Updates to Apply, with Newer Updates at the Beginning.
type PriceUpdateQueue
    = [(Currency, Quantity)]

-- | A Mapping From Currencies to Their Calculated Data
type CurrencyCache
    = Map.Map Currency CurrencyData

-- | Data We Have Calculated from the `Trade`s & Prices.
data CurrencyData
    = CurrencyData
        { cCostBasis :: Quantity
        , cTotalQuantity :: Quantity
        , cTotalCost :: Quantity
        , cPrice :: Maybe Quantity
        , cPriceChange :: Maybe Rational
        , cCurrentValue :: Maybe Quantity
        , cGainLoss :: Maybe Quantity
        }

data AggregateData
    = AggregateData
        { aTotalCost :: Quantity
        , aTotalValue :: Quantity
        , aGainLoss :: Quantity
        , aTotalChange :: Rational
        }



-- INITIALIZATION

-- | Create the app's `TVar`s, then asynchronously load the trades & build
-- the Currency & Price caches.
initialState :: IO AppState
initialState = do
    (transactionsTVar, currencyTVar, priceTVar) <- atomically
        $ (,,) <$> newTVar [] <*> newTVar initialCache <*> newTVar []
    transactions <- readTradeTableExport "trade_table.csv"
    void . forkIO . atomically $ do
        writeTVar transactionsTVar transactions
        buildCurrencyCache currencyTVar transactions
    gdaxThread <- forkIO . GDAX.connect $ \priceString ->
        atomically
            $ modifyTVar priceTVar
            $ (:) (eth, readDecimalQuantity priceString)
    let currencies = nub . map tradeBuyCurrency $ getTrades transactions
    binanceThreads <- forM currencies $ \c@(Currency symbol) ->
        forkIO . Binance.connect symbol $ \priceString ->
            atomically
                $ modifyTVar priceTVar
                $ (:) (c, readDecimalQuantity priceString)
    return AppState
        { appTransactions = []
        , appCurrencyCache = Map.empty
        , appAggregateData = AggregateData 0 0 0 0
        , appCacheTVars = (transactionsTVar, currencyTVar, priceTVar)
        , appPriceThreads = gdaxThread : binanceThreads
        }
    where
        -- | Add dummy ETH data since it's not added by buildCurrencyCache
        -- TODO: Might be easier to add separate ETH price to AppState or
        -- make a BTC/LTC/ETH->USD cache.
        initialCache :: CurrencyCache
        initialCache = Map.fromList
            [ ( eth
              , CurrencyData
                { cCostBasis = 0
                , cTotalQuantity = 0
                , cTotalCost = 0
                , cPrice = Nothing
                , cPriceChange = Nothing
                , cCurrentValue = Nothing
                , cGainLoss = Nothing
                }
              )
            ]
        getTrades :: [Transaction] -> [TradeData]
        getTrades =
            mapMaybe $ flip (.) transactionData $ \case
                Trade t ->
                    Just t
                _ ->
                    Nothing





-- | Build the `CurrencyCache` using a list of Transactions.
-- TODO: Handle income & expenses, build the FIFO queue, calculate realized
-- gains.
buildCurrencyCache :: TVar CurrencyCache -> [Transaction] -> STM ()
buildCurrencyCache cacheTVar transactions = do
    cache <- readTVar cacheTVar
    writeTVar cacheTVar $ foldl newTransaction cache transactions
    where
        newTransaction :: CurrencyCache -> Transaction -> CurrencyCache
        newTransaction cache Transaction {transactionData} =
            case transactionData of
                Trade t ->
                    let
                        currency = tradeBuyCurrency t
                        newVal = updateOrBuildData t $ Map.lookup currency cache
                    in
                        Map.insert currency newVal cache
                _ ->
                    cache

        -- TODO: Build a FIFO queue w/ a currencies buy amt & price, use to
        -- build cache after parsing all Transactions.
        updateOrBuildData :: TradeData -> Maybe CurrencyData -> CurrencyData
        updateOrBuildData trade = \case
            Nothing ->
                CurrencyData
                    { cCostBasis = tradeSellQuantity trade / tradeBuyQuantity trade
                    , cTotalQuantity = tradeBuyQuantity trade
                    , cTotalCost = tradeSellQuantity trade
                    , cPrice = Nothing
                    , cPriceChange = Nothing
                    , cCurrentValue = Nothing
                    , cGainLoss = Nothing
                    }
            Just cData ->
                CurrencyData
                    { cCostBasis =
                        (cCostBasis cData * cTotalQuantity cData + tradeSellQuantity trade)
                            / (cTotalQuantity cData + tradeBuyQuantity trade)
                    , cTotalQuantity =
                        cTotalQuantity cData + tradeBuyQuantity trade
                    , cTotalCost =
                        cCostBasis cData * cTotalQuantity cData + tradeSellQuantity trade
                    , cPrice = Nothing
                    , cPriceChange = Nothing
                    , cCurrentValue = Nothing
                    , cGainLoss = Nothing
                    }



-- UPDATE

-- | The Application-Specific Events
data AppEvent
    = Tick


-- | Update the State on Key Events & `Tick`s.
update :: AppState -> BrickEvent AppWidget AppEvent -> EventM AppWidget (Next AppState)
update s = \case
    VtyEvent ev ->
        case ev of
            V.EvKey (V.KChar 'q') [] ->
                liftIO (mapM killThread $ appPriceThreads s) >> halt s
            _ ->
                continue s
    AppEvent appEv ->
        case appEv of
            Tick ->
                liftIO (updateFromCaches s) >>= continue

    _ ->
        continue s


-- | Update the Application's State Using the `appCacheTVars`.
updateFromCaches :: AppState -> IO AppState
updateFromCaches s = atomically $ do
    let (transactionsTVar, currencyTVar, priceTVar) = appCacheTVars s
    (transactions, currencyCache, priceQueue) <-
        (,,)
            <$> readTVar transactionsTVar
            <*> readTVar currencyTVar
            <*> readTVar priceTVar
    let priceUpdates = nubBy (\(x,_) (y,_) -> x == y) priceQueue
        newCache = foldr updateCachePrice currencyCache priceUpdates
    writeTVar currencyTVar newCache
    writeTVar priceTVar []
    return s
        { appTransactions = transactions
        , appCurrencyCache = newCache
        , appAggregateData = calculateAggregates newCache
        }
    where
        -- Update the Price & Calculations for a Currency
        updateCachePrice :: (Currency, Quantity) -> CurrencyCache -> CurrencyCache
        updateCachePrice (currency, price) =
            let
                currentValue cData = cTotalQuantity cData * price
            in
                Map.adjust
                    (\cData -> cData
                        { cPrice = Just price
                        , cPriceChange = Just $ percentChange (cCostBasis cData) price
                        , cCurrentValue = Just $ currentValue cData
                        , cGainLoss = Just $ currentValue cData - cTotalCost cData
                        }

                    )
                    currency
        calculateAggregates :: CurrencyCache -> AggregateData
        calculateAggregates cache =
            let
                (costs, value) =
                    Map.foldl collectCostAndValue (0, 0) cache
            in
                AggregateData
                    { aTotalCost = costs
                    , aTotalValue = value
                    , aGainLoss = value - costs
                    , aTotalChange =
                        if costs == 0 then
                            0
                        else
                            fromQuantity $ (value - costs) / costs * 100
                    }
            where
                collectCostAndValue (cost, value) cData =
                    (cost + cTotalCost cData, value + fromMaybe 0 (cCurrentValue cData))
        percentChange :: Quantity -> Quantity -> Rational
        percentChange (Quantity original) (Quantity new) =
            (new - original) / original * 100



-- RENDER

-- | Represents the Unique Identifiers of Widgets Used in the Application.
type AppWidget
    = ()


-- | Render the Ethereum Gains Table
--
-- The table is built by stacking cells into columns, and then placing the
-- rows side-by-side. This is done to ensure every cell in a column expands
-- to the full width.
--
-- Cells are limited to a height of 1 line.
view :: AppState -> [Widget AppWidget]
view s =
    [ vBox
        [ B.hBorderWithLabel (str " Ethereum Gains ")
        , B.border
            $ padLeftRight 1
            $ hBox . map (vBox . map (vLimit 1)) $ transpose
                $ tableHeader
                : replicate (length tableHeader) B.hBorder
                : reverse (Map.foldlWithKey tableRow [] (appCurrencyCache s))
                ++ tableFooter s
        , statusBar s
        ]
    ]


-- | Render a Simple Status Bar Showing the Current USD Price for ETH.
statusBar :: AppState -> Widget AppWidget
statusBar s =
    padLeft Max
        $ padRight (Pad 1)
        $ str
        $ maybe "Loading GDAX Stream..." (("ETH-USD: $" ++) . showQuantity 2)
        $ cPrice <=< Map.lookup eth
        $ appCurrencyCache s


-- | Render the Header for the Ethereum Gains Table
tableHeader :: [Widget AppWidget]
tableHeader =
    [ centeredString "Currency"
    , alignRight "Total Quantity"
    , alignRight "Cost Per Unit"
    , alignRight "Current Price"
    , alignRight "% Change"
    , alignRight "Total Cost"
    , alignRight "Current Value"
    , alignRight "Gain / Loss"
    ]


-- | Render a Currency's Row in the Ethereum Gains Table
tableRow :: [[Widget AppWidget]] -> Currency -> CurrencyData -> [[Widget AppWidget]]
tableRow ws currency CurrencyData { cTotalQuantity, cCostBasis, cPrice, cPriceChange, cTotalCost, cCurrentValue, cGainLoss } =
        emptyIfEth
        [ centeredString $ show currency
        , alignRight $ show cTotalQuantity
        , alignRight $ show cCostBasis
        , alignRight $ maybe "Loading..." show cPrice
        , alignRight $ maybe "--" (showRational 2) cPriceChange
        , alignRight $ show cTotalCost
        , alignRight $ maybeToText cCurrentValue
        , alignRight $ maybeToText cGainLoss
        ]
        : ws
    where
        -- Don't render a row for ETH
        emptyIfEth row =
            if currency == eth then [] else row
        maybeToText =
            maybe "--" show

-- | Render the Totals Row of the Table
tableFooter :: AppState -> [[Widget AppWidget]]
tableFooter AppState { appCurrencyCache, appAggregateData } =
    [ replicate (length tableHeader) B.hBorder
    , [ str ""
      , str ""
      , str ""
      , alignRight "Totals:"
      , alignRight $ showRational 2 $ aTotalChange appAggregateData
      , alignRight $ show $ aTotalCost appAggregateData
      , alignRight $ show $ aTotalValue appAggregateData
      , alignRight $ show $ aGainLoss appAggregateData
      ]
    , [ str ""
      , str ""
      , str ""
      , str ""
      , str ""
      , inUSD $ aTotalCost appAggregateData
      , inUSD $ aTotalValue appAggregateData
      , inUSD $ aGainLoss appAggregateData
      ]
    ]
    where inUSD d =
            alignRight
                . maybe "Loading..." (("$" ++) . showQuantity 2 . (* d))
                $ cPrice =<< Map.lookup eth appCurrencyCache

-- | Center a String
centeredString :: String -> Widget n
centeredString = C.center . str

-- | Align a String to the Right of it's Parent Widget.
alignRight :: String -> Widget n
alignRight = padLeft Max . str



-- STYLE

-- | The Style Map for the Application.
--
-- Currently we only use the terminal's default values.
styles :: AppState -> AttrMap
styles _ =
    attrMap V.defAttr
        [
        ]
