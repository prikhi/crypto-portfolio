{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module App where


import Brick hiding (on)
import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent (ThreadId, forkIO, threadDelay, killThread)
import Control.Concurrent.STM (modifyTVar)
import Control.Monad (forever, forM)
import Control.Monad.IO.Class (liftIO)
import Data.Function (on)
import Data.List (nub, nubBy, sortBy)
import Data.Maybe (listToMaybe, mapMaybe)
import GHC.Conc (TVar, newTVarIO, readTVar, writeTVar, atomically)

import qualified Graphics.Vty as V

import Binance
import CoinTracking
import GDAX
import Types
import qualified EthereumGains
import qualified TradeList


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
        , appCacheTVars :: TVar PriceUpdateQueue
        , appPriceThreads :: [ThreadId]
        , appCurrentView :: AppView
        , appViewData :: ViewData
        }

-- | A List of Price Updates to Apply, with Newer Updates at the Beginning.
type PriceUpdateQueue
    = [(Currency, Quantity)]

data AppView
    = EthereumGains
    | TradeList
    deriving (Bounded, Enum, Eq)

newtype ViewData
    = ViewData
        { vdEthereumGains :: EthereumGains.State
        }



-- INITIALIZATION

-- | Create the app's `TVar`s, then asynchronously load the trades & build
-- the Currency & Price caches.
initialState :: IO AppState
initialState = do
    priceTVar <- newTVarIO []
    transactions <-
            sortBy (flip compare `on` transactionDate)
                <$> readTradeTableExport "trade_table.csv"
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
        { appTransactions = transactions
        , appCacheTVars = priceTVar
        , appPriceThreads = gdaxThread : binanceThreads
        , appCurrentView = EthereumGains
        , appViewData = initialViewData transactions
        }
    where
        initialViewData :: [Transaction] -> ViewData
        initialViewData ts =
            ViewData $ EthereumGains.initial ts
        getTrades :: [Transaction] -> [TradeData]
        getTrades ts =
            flip mapMaybe (EthereumGains.getTransactions ts)
                $ \tr -> case transactionData tr of
                    Trade t ->
                        Just t
                    _ ->
                        Nothing



-- UPDATE

-- | The Application-Specific Events
data AppEvent
    = Tick

-- | Update the State on Key Events & `Tick`s.
-- TODO: c-n/p switch view, on switch to eth gains, update currency cache
update :: AppState -> BrickEvent AppWidget AppEvent -> EventM AppWidget (Next AppState)
update s = \case
    VtyEvent ev ->
        case ev of
            V.EvKey (V.KChar 'q') [] ->
                liftIO (mapM killThread $ appPriceThreads s) >> halt s
            V.EvKey (V.KChar 'n') [] ->
                continue s
                    { appCurrentView = nextBoundedEnum $ appCurrentView s
                    }
            V.EvKey (V.KChar 'p') [] ->
                continue s
                    { appCurrentView = previousBoundedEnum $ appCurrentView s
                    }
            _ ->
                case appCurrentView s of
                    TradeList ->
                        TradeList.update ev >> continue s
                    EthereumGains ->
                        continue s
    AppEvent appEv ->
        case appEv of
            Tick ->
                liftIO (updateFromCaches s) >>= continue

    _ ->
        continue s
    where
        nextBoundedEnum e =
            if e == maxBound then
                minBound
            else
                succ e
        previousBoundedEnum e =
            if e == minBound then
                maxBound
            else
                pred e


-- | Update the Application's State Using the `appCacheTVars`.
updateFromCaches :: AppState -> IO AppState
updateFromCaches s = atomically $ do
    let priceTVar = appCacheTVars s
    priceQueue <- readTVar priceTVar
    writeTVar priceTVar []
    let priceUpdates = nubBy (\(x,_) (y,_) -> x == y) priceQueue
    return s
        { appViewData = (appViewData s)
            { vdEthereumGains =
                EthereumGains.updateCacheAndAggregate
                    (vdEthereumGains (appViewData s))
                    priceUpdates
            }
        }



-- RENDER

-- | Render the Ethereum Gains Table
--
-- The table is built by stacking cells into columns, and then placing the
-- rows side-by-side. This is done to ensure every cell in a column expands
-- to the full width.
--
-- Cells are limited to a height of 1 line.
view :: AppState -> [Widget AppWidget]
view s =
    case appCurrentView s of
        EthereumGains ->
            EthereumGains.view . vdEthereumGains $ appViewData s
        TradeList ->
            TradeList.view $ appTransactions s



-- STYLE

-- | The Style Map for the Application.
--
-- Currently we only use the terminal's default values.
styles :: AppState -> AttrMap
styles _ =
    attrMap V.defAttr
        [
        ]
