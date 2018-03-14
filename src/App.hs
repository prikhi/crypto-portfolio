{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module App where

import Brick
import Data.List (foldl')
import Data.Maybe (listToMaybe)
import Data.Ratio (numerator, denominator)

import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Data.Map as Map
import qualified Graphics.Vty as V

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

initialState :: AppState
initialState =
    let
        trades =
            map (\x -> x ETH)
                [ Trade (Quantity 1.5) BNB (Quantity 0.017448)
                , Trade (Quantity 2.23) BNB (Quantity 0.02428247)
                , Trade (Quantity 0.16) NEO (Quantity 0.02086320)
                , Trade (Quantity 0.344655) XMR (Quantity 0.10890615)
                ]
    in
        AppState
            { appTrades = trades
            , appCurrencyCache = buildCache trades
            }

data Trade
    = Trade
        { tBuyQuantity :: Quantity
        , tBuyCurrency :: Currency
        , tSellQuantity :: Quantity
        , tSellCurrency :: Currency
        }

type CurrencyCache
    = Map.Map Currency CurrencyData

data CurrencyData
    = CurrencyData
        { cCostBasis :: Quantity
        , cTotalQuantity :: Quantity
        }

buildCache :: [Trade] -> CurrencyCache
buildCache =
    foldl' newTrade Map.empty
    where
        newTrade cache trade =
            Map.alter
                (\case
                    Nothing ->
                        Just CurrencyData
                            { cCostBasis = tSellQuantity trade / tBuyQuantity trade
                            , cTotalQuantity = tBuyQuantity trade
                            }
                    Just cData ->
                        Just CurrencyData
                            { cCostBasis = (cCostBasis cData * cTotalQuantity cData + tSellQuantity trade) / (cTotalQuantity cData + tSellQuantity trade)
                            , cTotalQuantity = cTotalQuantity cData + tBuyQuantity trade
                            }
                )
                (tBuyCurrency trade)
                cache


-- Fields

-- TODO: Make Integer Instead of Rational, Need to Figure Out Atomic Units
newtype Quantity
    = Quantity
        { fromQuantity :: Rational
        } deriving (Num, Fractional)

instance Show Quantity where
    show (Quantity rat) =
        (if num < 0 then "-" else "") ++ shows d ("." ++ fractional ++ replicate (len - length fractional) '0')
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
    deriving (Show, Enum, Bounded, Ord, Eq)


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
        --, centeredString "Current Price"
        --, centeredString "% Growth"
        --, centeredString "Gain / Loss"
        ]


tableRows :: [Widget AppWidget] -> Currency -> CurrencyData -> [Widget AppWidget]
tableRows ws currency cData =
    flip (:) ws
        $ vLimit 1 (hBox
            [ centeredString . show $ currency
            , centeredString . show $ cTotalQuantity cData
            , centeredString . show $ cCostBasis cData
            ])

centeredString :: String -> Widget n
centeredString = C.center . str



-- STYLE

styles :: AppState -> AttrMap
styles _ =
    attrMap V.defAttr
        [
        ]
