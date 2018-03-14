{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module App where

import Brick
import Control.Lens ((&), (^?), (.~))
import Data.Aeson.Lens (key, _String)
import Data.Foldable (foldlM)
import Data.Maybe (listToMaybe)
import Data.Ratio (numerator, denominator)
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

type AppEvent = ()

type AppWidget = ()



config :: App AppState AppEvent AppWidget
config =
    App
        { appDraw = view
        , appChooseCursor = const listToMaybe
        , appHandleEvent = update
        , appStartEvent = return
        , appAttrMap = styles
        }


data AppState
    = AppState
        { appTrades :: [Trade]
        , appCurrencyCache :: CurrencyCache
        }

initialState :: IO AppState
initialState = do
    trades <- loadTrades "eth_trades.csv"
    cache <- buildCache trades
    return AppState
        { appTrades = trades
        , appCurrencyCache = cache
        }

data Trade
    = Trade
        { tBuyQuantity :: Quantity
        , tBuyCurrency :: Currency
        , tSellQuantity :: Quantity
        , tSellCurrency :: Currency
        }


-- | Parse a Coin Tracking `Trade List` CSV Export
loadTrades :: String -> IO [Trade]
loadTrades fileName = do
    csvData <- L.drop 1 . LC.dropWhile (/= '\n') <$> L.readFile fileName
    case Csv.decode Csv.NoHeader csvData of
        Left err ->
            putStrLn err >> return []
        Right v ->
            return . Vec.toList . flip Vec.mapMaybe v $ \(_ :: T.Text, tradeType :: T.Text, buyAmount :: T.Text, buyCurrency :: T.Text, sellAmount :: T.Text, sellCurrency :: T.Text, _ :: T.Text, _ :: T.Text, _ :: T.Text, _ :: T.Text) ->
                if tradeType == "Trade" then
                    Just Trade
                        { tBuyQuantity = readDecimalQuantity $ T.unpack buyAmount
                        , tBuyCurrency = read $ T.unpack buyCurrency
                        , tSellQuantity = readDecimalQuantity $ T.unpack sellAmount
                        , tSellCurrency = read $ T.unpack sellCurrency
                        }
                else
                    Nothing

readDecimalQuantity :: String -> Quantity
readDecimalQuantity =
    Quantity . toRational . (read :: String -> Double)


type CurrencyCache
    = Map.Map Currency CurrencyData

data CurrencyData
    = CurrencyData
        { cCostBasis :: Quantity
        , cTotalQuantity :: Quantity
        , cCurrentPrice :: Maybe Quantity
        }


-- | Build the `CurrencyCache` using a list of Trades.
--
-- This will query Binance's API for the latest trading prices.
buildCache :: [Trade] -> IO CurrencyCache
buildCache =
    foldlM newTrade Map.empty
    where
        newTrade :: CurrencyCache -> Trade -> IO CurrencyCache
        newTrade cache trade = do
            let currency = tBuyCurrency trade
            newVal <- updateOrBuildData trade $ Map.lookup currency cache
            return $ Map.insert currency newVal cache

        updateOrBuildData :: Trade -> Maybe CurrencyData -> IO CurrencyData
        updateOrBuildData trade = \case
            Nothing -> do
                maybeCurrentPrice <- getBinancePrice $ tBuyCurrency trade
                return CurrencyData
                    { cCostBasis = tSellQuantity trade / tBuyQuantity trade
                    , cTotalQuantity = tBuyQuantity trade
                    , cCurrentPrice = maybeCurrentPrice
                    }
            Just cData ->
                return CurrencyData
                    { cCostBasis =
                        (cCostBasis cData * cTotalQuantity cData + tSellQuantity trade)
                            / (cTotalQuantity cData + tBuyQuantity trade)
                    , cTotalQuantity =
                        cTotalQuantity cData + tBuyQuantity trade
                    , cCurrentPrice = cCurrentPrice cData
                    }


-- Fields

-- TODO: Make Integer Instead of Rational, Need to Figure Out Atomic Units
newtype Quantity
    = Quantity
        { fromQuantity :: Rational
        } deriving (Num, Fractional)

instance Show Quantity where
    show (Quantity rat) =
        (if num < 0 then "-" else " ") ++ shows d ("." ++ fractional ++ replicate (len - length fractional) '0')
        where
            fractional = take len (go next)
            len = 8
            (d, next) = abs num `quotRem` den
            num = numerator rat
            den = denominator rat

            go 0 = ""
            go x = let (d_, next_) = (10 * x) `quotRem` den
                    in shows d_ (go next_)

data Currency
    = BNB
    | ETH
    | NANO
    | NEO
    | XMR
    | XRP
    deriving (Read, Show, Enum, Bounded, Ord, Eq)


-- UPDATE

update :: AppState -> BrickEvent AppWidget AppEvent -> EventM AppWidget (Next AppState)
update s = \case
    VtyEvent ev ->
        case ev of
            V.EvKey (V.KChar 'q') [] ->
                halt s
            _ ->
                continue s
    _ ->
        continue s


getBinancePrice :: Currency -> IO (Maybe Quantity)
getBinancePrice currency = do
    resp <-
        Wreq.asValue
            =<< Wreq.getWith
                (Wreq.defaults & Wreq.param "symbol" .~ [T.pack $ show currency ++ "ETH"])
                "https://api.binance.com/api/v1/ticker/24hr"
    let decoded = resp ^? Wreq.responseBody . key "lastPrice" . _String
    return . fmap (Quantity . toRational) $ ((readMaybe :: String -> Maybe Double) . T.unpack) =<< decoded



-- RENDER

view :: AppState -> [Widget AppWidget]
view s =
    [ vBox
        [ B.hBorderWithLabel (str " Etherium Gains ")
        , B.border
            $ vBox
                $ vLimit 1 tableHeader
                : B.hBorder
                : Map.foldlWithKey tableRows [] (appCurrencyCache s)
        ]
    ]


tableHeader :: Widget AppWidget
tableHeader =
    hBox
        [ centeredString "Currency"
        , centeredString "Total Quantity"
        , centeredString "Cost Per Unit"
        , centeredString "Current Price"
        , centeredString "% Change"
        , centeredString "Total Cost"
        , centeredString "Current Value"
        , centeredString "Gain / Loss"
        ]


-- TODO: Move Calculations out of here into a cache so we can easily do totals as well
tableRows :: [Widget AppWidget] -> Currency -> CurrencyData -> [Widget AppWidget]
tableRows ws currency cData =
    let
        maybePriceChange = percentChange (cCostBasis cData) <$> cCurrentPrice cData
        totalCost = cTotalQuantity cData * cCostBasis cData
        maybeCurrentValue = (* cTotalQuantity cData) <$> cCurrentPrice cData
    in
        flip (:) ws
            $ vLimit 1 (hBox
                [ centeredString . show $ currency
                , centeredString . show $ cTotalQuantity cData
                , centeredString . show $ cCostBasis cData
                , centeredString . maybe "Update Failure" show
                    $ cCurrentPrice cData
                , centeredString $ maybe "--" show maybePriceChange
                , centeredString $ show totalCost
                , centeredString $ maybe "--" show maybeCurrentValue
                , centeredString . maybe "--" show
                    $ (\current -> current - totalCost) <$> maybeCurrentValue
                ])
    where percentChange original new =
            (new - original) / original * 100

centeredString :: String -> Widget n
centeredString = C.center . str



-- STYLE

styles :: AppState -> AttrMap
styles _ =
    attrMap V.defAttr
        [
        ]
