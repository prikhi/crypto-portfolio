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
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async)
import Control.Exception.Safe (catchAny)
import Control.Monad (forM)
import Data.List (nub)
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)

import qualified Graphics.Vty as V

import Binance
import Table
import Types
import qualified EthereumGains
import qualified GDAX
import qualified Styles
import qualified TradeList
import qualified USDGains


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
--
-- TODO: Combine all GDAX streams into a single, multi-currency stream.
-- TODO: Combine all Binance streams into a single, multi-currency stream.
priceUpdateChannel :: [Transaction] -> IO (BChan AppEvent, [Async ()])
priceUpdateChannel transactions = do
    channel <- newBChan 20
    gdaxThreads <- forM GDAX.currencies $
        priceUpdateThread channel GDAX.connect
    binanceThreads <- forM binanceCurrencies $
        priceUpdateThread channel Binance.connect
    return (channel, gdaxThreads ++ binanceThreads)
    where
        priceUpdateThread
            :: BChan AppEvent
            -> (Currency -> (String -> IO ()) -> IO ())
            -> Currency
            -> IO (Async ())
        priceUpdateThread channel getPrice currency =
            async $ retryForever $ getPrice currency $ \priceString ->
                writeBChan channel . PriceUpdate currency
                    $ readDecimalQuantity priceString
        retryForever :: IO a -> IO a
        retryForever action = catchAny action . const $
            threadDelay 500000 >> retryForever action
        binanceCurrencies :: [Currency]
        binanceCurrencies =
            filter (`notElem` GDAX.currencies) . nub . concat
                $ mapMaybe getCurrencies transactions
        getCurrencies :: Transaction -> Maybe [Currency]
        getCurrencies t =
            case transactionData t of
                Trade td ->
                    Just $
                        [ tradeBuyCurrency td
                        , tradeSellCurrency td
                        ] ++ maybeToList (tradeFeeCurrency td)
                Income d ->
                    Just $ incomeCurrency d : maybeToList (incomeFeeCurrency d)
                _ ->
                    Nothing



-- MODEL

-- | The data available to the application UI when rendering & responding
-- to actions.
data AppState
    = AppState
        { appTransactions :: [Transaction]
        , appCurrentView :: AppView
        , appViewData :: ViewData
        }

-- | The different views/screens available in the application.
data AppView
    = EthereumGains
    | USDGains
    | TradeList
    deriving (Bounded, Enum, Eq)

-- | The data associated with each `AppView`.
data ViewData
    = ViewData
        { vdEthereumGains :: EthereumGains.State
        , vdUSDGains :: USDGains.State
        }

-- | All custom events the application may respond to.
data AppEvent
    = PriceUpdate Currency Quantity     -- ^ Sent When a New GDAX/Binance Price is Received



-- INITIALIZATION

-- | Initialize each View's data & set the default View.
initialState :: [Transaction] -> IO AppState
initialState transactions = do
    usdGainsInitial <- USDGains.initial transactions
    return AppState
        { appTransactions = transactions
        , appCurrentView = EthereumGains
        , appViewData = initialViewData usdGainsInitial
        }
    where
        initialViewData :: USDGains.State -> ViewData
        initialViewData usdgInitial =
            ViewData
                { vdEthereumGains = EthereumGains.initial transactions
                , vdUSDGains = usdgInitial
                }



-- UPDATE

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
                    USDGains ->
                        updateTable USDGainsTable ev >> continue s

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
                , vdUSDGains =
                    USDGains.updatePrice
                        (vdUSDGains (appViewData s)) currency quantity
                }



-- RENDER

-- | Render the Currently Selected View.
view :: AppState -> [Widget AppWidget]
view s =
    case appCurrentView s of
        EthereumGains ->
            EthereumGains.view . vdEthereumGains $ appViewData s
        USDGains ->
            USDGains.view . vdUSDGains $ appViewData s
        TradeList ->
            TradeList.view $ appTransactions s
