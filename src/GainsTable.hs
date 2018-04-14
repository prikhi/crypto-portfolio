{-# LANGUAGE TemplateHaskell #-}
{-|

A Gains Table calculates & displays cost/value/gain information for
a Currency after a sequence of Transactions.

The table maintains a `Queues` map and applies a transformation for each
Transaction & then uses the final map to generate the column & total
values.

Each rendered Currency row contains:

* Currency
* Total Quantity
* Cost Per Unit
* Current Price
* % Change
* Total Cost
* Current Value
* Unrealized Gains
* Realized Gains

-}
module GainsTable
    ( GainsState
    , currencyCache
    , aggregateData
    , CurrencyCache
    , CurrencyData
    , getPrice
    , AggregateData(..)
    , Queues
    -- Initialize / Update
    , initialGainsState
    , updateGainsState
    , updateCachePrice
    , calculateAggregates
    -- Render
    , renderGainsTable
    , tableColumns
    , totalsRow
    ) where

import Brick (Widget, Padding(..), padLeft, str)
import Control.Lens ((&), (^.), (.~), makeLenses)
import Data.Maybe (fromMaybe)

import qualified Data.Map as Map

import Table
import Types
import qualified QuantityQueue as QQ


-- | The Fetched & Calculated Data for a Currency.
data CurrencyData
    = CurrencyData
        { cTotalQuantity :: Quantity
        , cTotalCost :: Quantity
        , cCostBasis :: Quantity
        , cPrice :: Maybe Quantity
        , cPriceChange :: Maybe Rational
        , cCurrentValue :: Maybe Quantity
        , cUnrealizedGains :: Maybe Quantity
        , cRealizedGains :: Quantity
        } deriving Show

-- | Aggregate Cost & Value Information Over All Currencies.
data AggregateData
    = AggregateData
        { aTotalCost :: Quantity
        , aTotalValue :: Quantity
        , aUnrealizedGains :: Quantity
        , aRealizedGains :: Quantity
        , aTotalChange :: Rational
        } deriving Show

-- | A Mapping From Currencies to Their Calculated Data
type CurrencyCache
    = Map.Map Currency CurrencyData

-- | Get the price for a Currency in the Cache.
getPrice :: Currency -> CurrencyCache -> Maybe Quantity
getPrice c cache =
    Map.lookup c cache >>= cPrice

-- | FIFO Queues for Currencies
type Queues = Map.Map Currency QQ.Queue

-- | The data used to render a Gains table.
data GainsState
    = GainsState
        { _currencyCache :: CurrencyCache
        , _aggregateData :: AggregateData
        }
makeLenses ''GainsState

-- | Build the initial state for the Gains Table using a queue updating
-- function & some Transactions.
initialGainsState :: (Transaction -> Queues -> Queues) -> [Transaction] -> GainsState
initialGainsState queueUpdater transactions =
    let
        cache = buildCache queueUpdater transactions
    in
        GainsState
            { _currencyCache = cache
            , _aggregateData = AggregateData 0 0 0 0 0
            }

-- | Build a Cache by Iteratively Updating QuantityQueues & Summing the
-- Final Results.
buildCache :: (Transaction -> Queues -> Queues) -> [Transaction] -> CurrencyCache
buildCache queueUpdater transactions =
    buildTotals <$> foldl (flip queueUpdater) Map.empty transactions

-- | Build a `CurrencyData` with a Currency's Queue.
buildTotals :: QQ.Queue -> CurrencyData
buildTotals queue =
    let
        quantity =
            QQ.totalQuantity queue
        cost =
            QQ.totalCost queue
    in
        CurrencyData
            { cTotalQuantity = quantity
            , cTotalCost = cost
            , cCostBasis = cost / quantity
            , cPrice = Nothing
            , cPriceChange = Nothing
            , cCurrentValue = Nothing
            , cUnrealizedGains = Nothing
            , cRealizedGains = QQ.realizedGains queue
            }

-- UPDATE

-- | Update the Cache & Aggregates with a new Price.
updateGainsState :: Currency -> QQ.Price -> GainsState -> GainsState
updateGainsState currency price s =
    let
        updatedCache =
            updateCachePrice (s ^. currencyCache) currency price
    in
        s
            & currencyCache .~ updatedCache
            & aggregateData .~ calculateAggregates updatedCache


-- | Update the Cache with a new Price & recalculate the Currency's data.
updateCachePrice :: CurrencyCache -> Currency -> Quantity -> CurrencyCache
updateCachePrice cache currency price =
    let
        currentValue cData =
            cTotalQuantity cData * price
    in
        Map.adjust
            (\cData -> cData
                { cPrice = Just price
                , cPriceChange = percentChange (cCostBasis cData) price
                , cCurrentValue = Just $ currentValue cData
                , cUnrealizedGains = Just $ currentValue cData - cTotalCost cData
                }
            )
            currency
            cache
    where
        percentChange :: Quantity -> Quantity -> Maybe Rational
        percentChange (Quantity original) (Quantity new) =
            if original == 0 then
                Nothing
            else
                Just $ (new - original) / original * 100

-- | Calculate the Totals for a Cache.
calculateAggregates :: CurrencyCache -> AggregateData
calculateAggregates c =
    let
        (costs, value, realizedGain) =
            Map.foldl sumCurrencyData (0, 0, 0) c
    in
        AggregateData
            { aTotalCost = costs
            , aTotalValue = value
            , aUnrealizedGains = value - costs
            , aRealizedGains = realizedGain
            , aTotalChange =
                if costs == 0 then
                    0
                else
                    fromQuantity $ (value - costs) / costs * 100
            }
    where
        sumCurrencyData
            :: (Quantity, Quantity, Quantity)
            -> CurrencyData
            -> (Quantity, Quantity, Quantity)
        sumCurrencyData (cost, value, gain) cData =
            ( cost + cTotalCost cData
            , value + fromMaybe 0 (cCurrentValue cData)
            , gain + cRealizedGains cData
            )


-- RENDER

-- | Render a Gains Table with the given percision & additional footer
-- rows.
renderGainsTable
    :: (Ord n, Show n)
    => n
    -> Int
    -> [[Widget n]]
    -> GainsState
    -> [(Currency, CurrencyData)]
    -> Widget n
renderGainsTable n decimalPlaces extraFooterRows s =
    table TableConfig
        { columns =
            tableColumns decimalPlaces
        , footerRows =
            totalsRow decimalPlaces (s ^. aggregateData)
            : extraFooterRows
        , name =
            n
        , showRowDividers =
            True
        }

-- | A table column-specification for a Currency & it's calculated data.
--
-- All values are rounded to the given amount of decimal places, except for
-- the percent change, which is always given 2 decimal places.
tableColumns :: Int -> [Column (Currency, CurrencyData)]
tableColumns decimalPlaces =
    [ column
        { headerName = "Currency"
        , headerAlign = Alignment VMiddle HCenter
        , dataAlign = Alignment VMiddle HCenter
        , columnWeight = 1
        , dataSelector = show . fst
        }
    , column
        { headerName = "Total Quantity"
        , dataSelector = show . cTotalQuantity . snd
        }
    , column
        { headerName = "Cost Per Unit"
        , dataSelector = showQuantity decimalPlaces . cCostBasis . snd
        }
    , column
        { headerName = "Current Price"
        , dataSelector = maybe "Loading..." (showQuantity decimalPlaces) . cPrice . snd
        }
    , column
        { headerName = "% Change"
        , dataSelector = maybe "--" (showRational 2) . cPriceChange . snd
        , columnWeight = 5
        }
    , column
        { headerName = "Total Cost"
        , dataSelector = showQuantity decimalPlaces . cTotalCost . snd
        }
    , column
        { headerName = "Curent Value"
        , dataSelector = maybeText . cCurrentValue . snd
        }
    , column
        { headerName = "Unrealized Gains"
        , dataSelector = maybeText . cUnrealizedGains . snd
        , columnWeight = 13
        }
    , column
        { headerName = "Realized Gains"
        , dataSelector = showQuantity decimalPlaces . cRealizedGains . snd
        , columnWeight = 13
        }
    ]
    where
        maybeText =
            maybe "--" (showQuantity decimalPlaces)
        column =
            Column
                { headerName = ""
                , headerAlign = Alignment VMiddle HRight
                , dataAlign = Alignment VMiddle HRight
                , columnWeight = 10
                , dataSelector = const ""
                }


-- | Render the Aggregate Data as a Table Row.
totalsRow :: Int -> AggregateData -> [Widget n]
totalsRow decimalPlaces aggData =
    [ str " "
    , str " "
    , str " "
    , alignRight "Totals:"
    , alignRight $ showRational 2 $ aTotalChange aggData
    , alignRight $ showQuantity decimalPlaces $ aTotalCost aggData
    , alignRight $ showQuantity decimalPlaces $ aTotalValue aggData
    , alignRight $ showQuantity decimalPlaces $ aUnrealizedGains aggData
    , alignRight $ showQuantity decimalPlaces $ aRealizedGains aggData
    ]
    where
        alignRight :: String -> Widget n
        alignRight =
            padLeft Max . str
