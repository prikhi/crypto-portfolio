{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module App where


import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent (ThreadId, forkIO, threadDelay, killThread)
import Control.Concurrent.STM (modifyTVar)
import Control.Monad ((<=<), void, forever, mzero, forM)
import Control.Monad.IO.Class (liftIO)
import Data.Csv ((.!))
import Data.List (transpose, nub, nubBy)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Ratio (numerator, denominator)
import Data.Scientific (Scientific)
import GHC.Conc (TVar, STM, newTVar, readTVar, writeTVar, atomically)

import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Csv as Csv
import qualified Data.Map as Map
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V

import Binance
import GDAX


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
        { appTrades :: [Trade]
        , appCurrencyCache :: CurrencyCache
        , appAggregateData :: AggregateData
        , appCacheTVars :: (TVar [Trade], TVar CurrencyCache, TVar PriceUpdateQueue)
        , appPriceThreads :: [ThreadId]
        }


-- | A Trade of one `Currency` for `Another`
data Trade
    = Trade
        { tBuyQuantity :: Quantity
        , tBuyCurrency :: Currency
        , tSellQuantity :: Quantity
        , tSellCurrency :: Currency
        }

-- | Parse a Trade from a CoinTracking.Info `Trade List` Export
--
-- We have to use index-based parsing here because the export contains
-- 2 `"Cur."` columns
instance Csv.FromRecord Trade where
    parseRecord v =
        if length v == 10 then do
            trade <- v .! 1
            if trade == ("Trade" :: String) then
                Trade
                    <$> (readDecimalQuantity <$> v .! 2)
                    <*> (Currency <$> v .! 3)
                    <*> (readDecimalQuantity <$> v .! 4)
                    <*> (Currency <$> v .! 5)
            else
                mzero
        else
            mzero


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



-- FIELDS

-- | An Amount of a Coin that has been Bought/Sold, or a Per-Unit Price.
-- TODO: Make Integer Instead of Rational, Need to Figure Out Atomic Units
newtype Quantity
    = Quantity
        { fromQuantity :: Rational
        } deriving (Num, Fractional)

-- | Show 8 Decimal Places by Default.
instance Show Quantity where
    show = showQuantity 8

-- | Read a `Quantity` from a `Scientific`-formatted String
readDecimalQuantity :: String -> Quantity
readDecimalQuantity =
    Quantity . toRational . (read :: String -> Scientific)

-- | Render a `Quantity` with a Fixed Number of Decimal Places
showQuantity :: Int -> Quantity -> String
showQuantity decimalPlaces (Quantity rat) =
    showRational decimalPlaces rat

-- | Render a `Rational` with a Fixed Number of Decimal Places
showRational :: Int -> Rational -> String
showRational decimalPlaces rat =
    sign ++ shows wholePart ("." ++ fractionalString ++ zeroPadding)
    where
        sign =
            if num < 0 then
                "-"
            else
                ""
        fractionalString =
            take decimalPlaces (buildFractionalString fractionalPart)
        zeroPadding =
            replicate (decimalPlaces - length fractionalString) '0'
        (wholePart, fractionalPart) =
            abs num `quotRem` den
        num =
            numerator rat
        den =
            denominator rat
        buildFractionalString 0 =
            ""
        buildFractionalString fraction =
            let
                (digit, remainingFraction) =
                    (10 * fraction) `quotRem` den
            in
                shows digit (buildFractionalString remainingFraction)


-- | Used as an Identifier for CryptoCurrencies.
newtype Currency
    = Currency { toSymbol :: String }
    deriving (Ord, Eq)

-- | Currencies are Represented by their Ticker Symbol
instance Show Currency where
    show = toSymbol

-- | The `Currency` Representing Ethereum.
eth :: Currency
eth =
    Currency "ETH"



-- INITIALIZATION

-- | Create the app's `TVar`s, then asynchronously load the trades & build
-- the Currency & Price caches.
initialState :: IO AppState
initialState = do
    (tradesTVar, currencyTVar, priceTVar) <- atomically
        $ (,,) <$> newTVar [] <*> newTVar initialCache <*> newTVar []
    trades <- loadTrades "eth_trades.csv"
    void . forkIO . atomically $ do
        writeTVar tradesTVar trades
        buildCurrencyCache currencyTVar trades
    gdaxThread <- forkIO . GDAX.connect $ \priceString ->
        atomically
            $ modifyTVar priceTVar
            $ (:) (eth, readDecimalQuantity priceString)
    let currencies = nub $ map tBuyCurrency trades
    binanceThreads <- forM currencies $ \c@(Currency symbol) ->
        forkIO . Binance.connect symbol $ \priceString ->
            atomically
                $ modifyTVar priceTVar
                $ (:) (c, readDecimalQuantity priceString)
    return AppState
        { appTrades = []
        , appCurrencyCache = Map.empty
        , appAggregateData = AggregateData 0 0 0 0
        , appCacheTVars = (tradesTVar, currencyTVar, priceTVar)
        , appPriceThreads = gdaxThread : binanceThreads
        }
    where
          -- | Add dummy ETH data since it's not added by buildCurrencyCache
          -- TODO: Might be easier to add separate ETH price to AppState
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


-- | Parse a Coin Tracking `Trade List` CSV Export
loadTrades :: String -> IO [Trade]
loadTrades fileName = do
    csvData <- L.drop 1 . LC.dropWhile (/= '\n') <$> L.readFile fileName
    case Csv.decode Csv.NoHeader csvData of
        Left err ->
            putStrLn err >> return []
        Right v ->
            return $ Vec.toList v


-- | Build the `CurrencyCache` using a list of Trades.
buildCurrencyCache :: TVar CurrencyCache -> [Trade] -> STM ()
buildCurrencyCache cacheTVar trades = do
    cache <- readTVar cacheTVar
    writeTVar cacheTVar $ foldl newTrade cache trades
    where
        newTrade :: CurrencyCache -> Trade -> CurrencyCache
        newTrade cache trade =
            let
                currency = tBuyCurrency trade
                newVal = updateOrBuildData trade $ Map.lookup currency cache
            in
                Map.insert currency newVal cache

        updateOrBuildData :: Trade -> Maybe CurrencyData -> CurrencyData
        updateOrBuildData trade = \case
            Nothing ->
                CurrencyData
                    { cCostBasis = tSellQuantity trade / tBuyQuantity trade
                    , cTotalQuantity = tBuyQuantity trade
                    , cTotalCost = tSellQuantity trade
                    , cPrice = Nothing
                    , cPriceChange = Nothing
                    , cCurrentValue = Nothing
                    , cGainLoss = Nothing
                    }
            Just cData ->
                CurrencyData
                    { cCostBasis =
                        (cCostBasis cData * cTotalQuantity cData + tSellQuantity trade)
                            / (cTotalQuantity cData + tBuyQuantity trade)
                    , cTotalQuantity =
                        cTotalQuantity cData + tBuyQuantity trade
                    , cTotalCost =
                        cCostBasis cData * cTotalQuantity cData + tSellQuantity trade
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
    let (tradesTVar, currencyTVar, priceTVar) = appCacheTVars s
    (trades, currencyCache, priceQueue) <-
        (,,)
            <$> readTVar tradesTVar
            <*> readTVar currencyTVar
            <*> readTVar priceTVar
    let priceUpdates = nubBy (\(x,_) (y,_) -> x == y) priceQueue
        newCache = foldr updateCachePrice currencyCache priceUpdates
    writeTVar currencyTVar newCache
    writeTVar priceTVar []
    return s
        { appTrades = trades
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
                    , aTotalChange = fromQuantity $ (value - costs) / costs * 100
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
--
-- TODO: Add Totals for some columns
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
