{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module App where


import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent (ThreadId, forkIO, threadDelay, killThread)
import Control.Lens ((&), (^?), (.~))
import Control.Monad (void, forever, mzero)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson.Lens (key, _String)
import Data.Csv ((.!))
import Data.List (transpose)
import Data.Maybe (listToMaybe)
import Data.Ratio (numerator, denominator)
import Data.Scientific (Scientific)
import GHC.Conc (TVar, STM, newTVar, readTVar, readTVarIO, writeTVar, atomically)
import Text.Read (readMaybe)

import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Csv as Csv
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import qualified Network.Wreq as Wreq

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
        , appPriceCache :: PriceCache
        , appCacheTVars :: (TVar [Trade], TVar CurrencyCache, TVar PriceCache)
        , appGDAXThread :: ThreadId
        }


-- | Create the app's `TVar`s, then asynchronously load the trades & build
-- the Currency & Price caches.
initialState :: IO AppState
initialState = do
    (tradesTVar, currencyTVar, priceTVar) <- atomically
        $ (,,) <$> newTVar [] <*> newTVar Map.empty <*> newTVar Map.empty
    void . forkIO $ do
        trades <- loadTrades "eth_trades.csv"
        mapM_ forkIO
            $ atomically (writeTVar tradesTVar trades)
            : atomically (buildCurrencyCache currencyTVar trades)
            : map (updatePriceCache priceTVar . tBuyCurrency) trades
    gdaxThread <- forkIO . GDAX.connectAndSubscribe $ \priceString ->
        atomically $ do
            cache <- readTVar priceTVar
            let price = readDecimalQuantity priceString
                newCache = Map.insert eth price cache
            writeTVar priceTVar newCache
    return AppState
        { appTrades = []
        , appCurrencyCache = Map.empty
        , appPriceCache = Map.empty
        , appCacheTVars = (tradesTVar, currencyTVar, priceTVar)
        , appGDAXThread = gdaxThread
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


-- | Parse a Coin Tracking `Trade List` CSV Export
loadTrades :: String -> IO [Trade]
loadTrades fileName = do
    csvData <- L.drop 1 . LC.dropWhile (/= '\n') <$> L.readFile fileName
    case Csv.decode Csv.NoHeader csvData of
        Left err ->
            putStrLn err >> return []
        Right v ->
            return $ Vec.toList v


-- | Read a `Quantity` from a `Scientific`-formatted String
readDecimalQuantity :: String -> Quantity
readDecimalQuantity =
    Quantity . toRational . (read :: String -> Scientific)


-- | A Mapping From Currencies to Their Calculated Data
type CurrencyCache
    = Map.Map Currency CurrencyData

-- | Data We Have Calculated from the `Trade`s.
data CurrencyData
    = CurrencyData
        { cCostBasis :: Quantity
        , cTotalQuantity :: Quantity
        }

-- | A Mapping from Currencies to Their Current Price per Unit.
type PriceCache
    = Map.Map Currency Quantity


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
                    }
            Just cData ->
                CurrencyData
                    { cCostBasis =
                        (cCostBasis cData * cTotalQuantity cData + tSellQuantity trade)
                            / (cTotalQuantity cData + tBuyQuantity trade)
                    , cTotalQuantity =
                        cTotalQuantity cData + tBuyQuantity trade
                    }

-- | Query Binance for a Currency's Price & Update the PriceCache.
updatePriceCache :: TVar PriceCache -> Currency -> IO ()
updatePriceCache cacheTVar currency = do
    maybePrice <- getBinancePrice currency
    case maybePrice of
        Just p ->
            atomically $ do
                cache <- readTVar cacheTVar
                let newCache = Map.insert currency p cache
                writeTVar cacheTVar newCache
        Nothing ->
            return ()


-- Fields

-- | An Amount of a Coin that has been Bought/Sold, or a Per-Unit Price.
-- TODO: Make Integer Instead of Rational, Need to Figure Out Atomic Units
newtype Quantity
    = Quantity
        { fromQuantity :: Rational
        } deriving (Num, Fractional)

-- | Show 8 Decimal Places by Default.
instance Show Quantity where
    show = showQuantity 8

-- | Render a `Quantity` with a Fixed Number of Decimal Places
showQuantity :: Int -> Quantity -> String
showQuantity decimalPlaces (Quantity rat) =
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
                liftIO (killThread $ appGDAXThread s) >> halt s
            V.EvKey (V.KChar 'r') [] ->
                liftIO (refreshPriceCache s) >> continue s
            _ ->
                continue s
    AppEvent appEv ->
        case appEv of
            Tick ->
                liftIO (updateFromCaches s) >>= continue

    _ ->
        continue s


-- | Asynchronously Pull the Latest Binance Prices Into the Cache.
refreshPriceCache :: AppState -> IO ()
refreshPriceCache s = do
    let (tradesTVar, _, priceTVar) = appCacheTVars s
    trades <- readTVarIO tradesTVar
    mapM_ (forkIO . updatePriceCache priceTVar . tBuyCurrency) trades


-- | Update the Application's State Using the `appCacheTVars`.
updateFromCaches :: AppState -> IO AppState
updateFromCaches s = atomically $ do
    let (tradesTVar, currencyTVar, priceTVar) = appCacheTVars s
    (trades, currency, price) <-
        (,,)
            <$> readTVar tradesTVar
            <*> readTVar currencyTVar
            <*> readTVar priceTVar
    return s
        { appTrades = trades
        , appCurrencyCache = currency
        , appPriceCache = price
        }


-- | Query Binance for a Currency's Last Sale Price in Ethereum-per-coin.
getBinancePrice :: Currency -> IO (Maybe Quantity)
getBinancePrice currency = do
    resp <-
        Wreq.getWith
            (Wreq.defaults & Wreq.param "symbol" .~ [T.pack $ show currency ++ "ETH"])
            "https://api.binance.com/api/v1/ticker/24hr"
            >>= Wreq.asValue
    let decoded = resp ^? Wreq.responseBody . key "lastPrice" . _String
    return . fmap (Quantity . toRational) $ ((readMaybe :: String -> Maybe Double) . T.unpack) =<< decoded



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
                : reverse (Map.foldlWithKey (tableRow $ appPriceCache s) [] (appCurrencyCache s))
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
        $ Map.lookup eth
        $ appPriceCache s


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
-- TODO: Move Calculations out of here into a cache so we can easily do totals as well
tableRow :: PriceCache -> [[Widget AppWidget]] -> Currency -> CurrencyData -> [[Widget AppWidget]]
tableRow priceCache ws currency cData =
    let
        maybePrice = Map.lookup currency priceCache
        maybePriceChange = percentChange (cCostBasis cData) <$> maybePrice
        totalCost = cTotalQuantity cData * cCostBasis cData
        maybeCurrentValue = (* cTotalQuantity cData) <$> maybePrice
    in
        [ centeredString $ show currency
        , alignRight . show $ cTotalQuantity cData
        , alignRight . show $ cCostBasis cData
        , alignRight $ maybe "Loading..." show maybePrice
        , alignRight $ maybe "--" (showQuantity 2) maybePriceChange
        , alignRight $ show totalCost
        , alignRight $ maybeToText maybeCurrentValue
        , alignRight $ maybeToText
            $ (\current -> current - totalCost) <$> maybeCurrentValue
        ]
        : ws
    where
        maybeToText =
            maybe "--" show
        percentChange original new =
            (new - original) / original * 100

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
