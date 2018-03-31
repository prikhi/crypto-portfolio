{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module EthereumGains where

import Brick
import Control.Monad ((<=<))
import Data.List (transpose)
import Data.Maybe (mapMaybe, fromMaybe)

import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Data.Map as Map

import Types

data State
    = State
        { currencyCache :: CurrencyCache
        , aggregateData :: AggregateData
        }

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

initial :: [Transaction] -> State
initial ts =
    let
        cache = buildCurrencyCache initialCache $ getTransactions ts
    in
        State
            { currencyCache = cache
            , aggregateData = AggregateData 0 0 0 0
            }
    where
        -- | Add dummy ETH data since it's not added by buildCurrencyCache
        -- TODO: Might be easier to add separate ETH price to AppState or
        -- make a BTC/LTC/ETH->USD cache in AppState.
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

-- | Filter out non-Ethereum or USD-ETH Transactions.
getTransactions :: [Transaction] -> [Transaction]
getTransactions =
    mapMaybe $ \transaction -> case transactionData transaction of
        Trade t ->
            if (tradeSellCurrency t == eth || tradeBuyCurrency t == eth)
                    && tradeSellCurrency t /= Currency "USD" then
                Just transaction
            else
                Nothing
        _ ->
            Nothing


buildCurrencyCache :: CurrencyCache -> [Transaction] -> CurrencyCache
buildCurrencyCache =
    foldl newTransaction
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

updateCacheAndAggregate :: State -> [(Currency, Quantity)] -> State
updateCacheAndAggregate s priceUpdates =
    let
        newCache = foldr updateCachePrice (currencyCache s) priceUpdates
    in
        s
            { currencyCache = newCache
            , aggregateData = calculateAggregates newCache
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


view :: State -> [Widget n]
view s =
    [ vBox
        [ B.hBorderWithLabel (str " Ethereum Gains ")
        , B.border
            $ padLeftRight 1
            $ hBox . map (vBox . map (vLimit 1)) $ transpose
                $ tableHeader
                : replicate (length tableHeader) B.hBorder
                : reverse (Map.foldlWithKey tableRow [] (currencyCache s))
                ++ tableFooter s
        , statusBar s
        ]
    ]


-- | Render a Simple Status Bar Showing the Current USD Price for ETH.
statusBar :: State -> Widget n
statusBar s =
    padLeft Max
        $ padRight (Pad 1)
        $ str
        $ maybe "Loading GDAX Stream..." (("ETH-USD: $" ++) . showQuantity 2)
        $ cPrice <=< Map.lookup eth
        $ currencyCache s


-- | Render the Header for the Ethereum Gains Table
tableHeader :: [Widget n]
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
tableRow :: [[Widget n]] -> Currency -> CurrencyData -> [[Widget n]]
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
tableFooter :: State -> [[Widget n]]
tableFooter State { currencyCache, aggregateData } =
    [ replicate (length tableHeader) B.hBorder
    , [ str ""
      , str ""
      , str ""
      , alignRight "Totals:"
      , alignRight $ showRational 2 $ aTotalChange aggregateData
      , alignRight $ show $ aTotalCost aggregateData
      , alignRight $ show $ aTotalValue aggregateData
      , alignRight $ show $ aGainLoss aggregateData
      ]
    , [ str ""
      , str ""
      , str ""
      , str ""
      , str ""
      , inUSD $ aTotalCost aggregateData
      , inUSD $ aTotalValue aggregateData
      , inUSD $ aGainLoss aggregateData
      ]
    ]
    where inUSD d =
            alignRight
                . maybe "Loading..." (("$" ++) . showQuantity 2 . (* d))
                $ cPrice =<< Map.lookup eth currencyCache


-- | Center a String
centeredString :: String -> Widget n
centeredString = C.center . str

-- | Align a String to the Right of it's Parent Widget.
alignRight :: String -> Widget n
alignRight = padLeft Max . str
