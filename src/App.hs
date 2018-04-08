{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module App
    ( config
    , priceUpdateChannel
    , initialState
    ) where


import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Exception (IOException, catch)
import Control.Monad (forM)
import Data.List (nub)
import Data.Maybe (listToMaybe, mapMaybe)

import qualified Graphics.Vty as V

import Binance
import GDAX
import Table
import Types
import qualified EthereumGains
import qualified TradeList
import qualified Styles


-- General Brick App Configuration & Initialization

-- | The Brick Configuration for the application.
config :: App AppState AppEvent AppWidget
config =
    App
        { appDraw = view
        , appChooseCursor = const listToMaybe
        , appHandleEvent = update
        , appStartEvent = return
        , appAttrMap = const Styles.attributeMap
        }

-- | Create a Brick Event Channel that receives PriceUpdate events from
-- forked GDAX & Binance Ticker Update Threads.
priceUpdateChannel :: [Transaction] -> IO (BChan AppEvent, [ThreadId])
priceUpdateChannel transactions = do
    channel <- newBChan 20
    gdaxThread <- priceUpdateThread channel eth GDAX.connect
    binanceThreads <- forM currencies $ \c@(Currency symbol) ->
        priceUpdateThread channel c (Binance.connect symbol)
    return (channel, gdaxThread : binanceThreads)
    where
        priceUpdateThread :: BChan AppEvent -> Currency -> ((String -> IO ()) -> IO ()) -> IO ThreadId
        priceUpdateThread channel currency getPrice =
            forkIO $ retryForever $ getPrice $ \priceString ->
                writeBChan channel . PriceUpdate currency $ readDecimalQuantity priceString
        retryForever :: IO a -> IO a
        retryForever action = catch action $ \(_ :: IOException) ->
            threadDelay 500000 >> retryForever action
        currencies :: [Currency]
        currencies =
            nub . map tradeBuyCurrency $ getTrades transactions
        getTrades :: [Transaction] -> [TradeData]
        getTrades ts =
            flip mapMaybe (EthereumGains.getTransactions ts)
                $ \tr -> case transactionData tr of
                    Trade t ->
                        Just t
                    _ ->
                        Nothing



-- MODEL

-- | The state of the application, including TVars as caches & asynchronous
-- update channels.
data AppState
    = AppState
        { appTransactions :: [Transaction]
        , appCurrentView :: AppView
        , appViewData :: ViewData
        }

data AppView
    = EthereumGains
    | TradeList
    deriving (Bounded, Enum, Eq)

newtype ViewData
    = ViewData
        { vdEthereumGains :: EthereumGains.State
        }



-- INITIALIZATION

-- | Initialize each View's data & set the default View.
initialState :: [Transaction] -> AppState
initialState transactions =
    AppState
        { appTransactions = transactions
        , appCurrentView = EthereumGains
        , appViewData = initialViewData
        }
    where
        initialViewData :: ViewData
        initialViewData =
            ViewData $ EthereumGains.initial transactions



-- UPDATE

-- | The Application-Specific Events
data AppEvent
    = PriceUpdate Currency Quantity

-- | Update the State on Key Events & `Tick`s.
update :: AppState -> BrickEvent AppWidget AppEvent -> EventM AppWidget (Next AppState)
update s = \case
    VtyEvent ev ->
        case ev of
            V.EvKey (V.KChar 'q') [] ->
                halt s
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
                        updateTable TradeListTable ev >> continue s
                    EthereumGains ->
                        updateTable EthereumGainsTable ev >> continue s

    AppEvent appEv ->
        case appEv of
            PriceUpdate currency newPrice ->
                continue $ handlePriceUpdate s currency newPrice

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

-- | Update the Application's State With a New Price for a Currency.
handlePriceUpdate :: AppState -> Currency -> Quantity -> AppState
handlePriceUpdate s currency quantity =
    s { appViewData = updatedViewData }
    where
        updatedViewData =
            (appViewData s)
                { vdEthereumGains =
                    EthereumGains.updatePrice
                        (vdEthereumGains (appViewData s)) currency quantity
                }



-- RENDER

-- | Render the Currently Selected View.
view :: AppState -> [Widget AppWidget]
view s =
    case appCurrentView s of
        EthereumGains ->
            EthereumGains.view . vdEthereumGains $ appViewData s
        TradeList ->
            TradeList.view $ appTransactions s
