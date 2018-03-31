{-# LANGUAGE LambdaCase #-}
module TradeList where

import Brick
import Data.List (transpose)
import Data.Maybe (fromMaybe)
import Data.Time.Format (formatTime, defaultTimeLocale)

import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Types



-- UPDATE

update :: V.Event -> EventM AppWidget ()
update = \case
    V.EvKey (V.KChar 'j') [] ->
        vScrollBy vp 1
    V.EvKey (V.KChar 'k') [] ->
        vScrollBy vp (-1)
    _ ->
        return ()


vp :: ViewportScroll AppWidget
vp =
    viewportScroll TradeListViewport


-- RENDER

view :: [Transaction] -> [Widget AppWidget]
view transactions =
    [ vBox
        [ B.hBorderWithLabel (str " Trade List ")
        , B.border
            $ viewport TradeListViewport Vertical
            $ padLeftRight 1
            $ hBox . map (vBox . map (vLimit 1)) $ transpose
                $ tableHeader
                : replicate (length tableHeader) B.hBorder
                : map tableRow transactions
        ]
    ]


tableHeader :: [Widget n]
tableHeader =
    [ centeredString "Type"
    , alignRight "Buy"
    , alignRight " "
    , alignRight "Sell"
    , alignRight " "
    , alignRight "Fee"
    , alignRight " "
    , centeredString "Exchange"
    , centeredString "Group"
    , centeredString "Comment"
    , centeredString "Date"
    ]


-- | TODO: Refactor some of these repeated function calls out!
tableRow :: Transaction -> [Widget n]
tableRow t =
    case transactionData t of
        Trade td ->
            [ centeredString "Trade"
            , alignRight
                $ showCurrencyQuantity (tradeBuyQuantity td) (tradeBuyCurrency td)
            , alignLeft . (" " ++) . show $ tradeBuyCurrency td
            , alignRight
                $ showCurrencyQuantity (tradeSellQuantity td) (tradeSellCurrency td)
            , alignLeft . (" " ++) . show $ tradeSellCurrency td
            , alignRight . fromMaybe " "
                $ showCurrencyQuantity <$> tradeFeeQuantity td <*> tradeFeeCurrency td
            , alignLeft . maybe " " ((" " ++) . show) $ tradeFeeCurrency td
            , centeredString $ T.unpack $ tradeExchange td
            , group
            , comment
            , date
            ]
        Income d ->
            [ centeredString "Income"
            , alignRight
                $ showCurrencyQuantity (incomeQuantity d) (incomeCurrency d)
            , alignLeft . (" " ++) . show $ incomeCurrency d
            , alignRight " "
            , alignRight " "
            , alignRight . fromMaybe " "
                $ showCurrencyQuantity <$> incomeFeeQuantity d <*> incomeFeeCurrency d
            , alignLeft . maybe " " ((" " ++) . show) $ incomeFeeCurrency d
            , centeredString $ T.unpack $ incomeExchange d
            , group
            , comment
            , date
            ]
        Expense ed ->
            [ centeredString "Expense"
            , alignRight " "
            , alignRight " "
            , alignRight
                $ showCurrencyQuantity (expenseQuantity ed) (expenseCurrency ed)
            , alignLeft . (" " ++) . show $ expenseCurrency ed
            , alignRight . fromMaybe " "
                $ showCurrencyQuantity <$> expenseFeeQuantity ed <*> expenseFeeCurrency ed
            , alignLeft . maybe " " ((" " ++) . show) $ expenseFeeCurrency ed
            , centeredString $ T.unpack $ expenseExchange ed
            , group
            , comment
            , date
            ]
        Transfer td ->
            [ centeredString "Transfer"
            , alignRight
                $ showCurrencyQuantity (transferBuyAmount td) (transferCurrency td)
            , alignLeft . (" " ++) . show $ transferCurrency td
            , alignRight
                $ showCurrencyQuantity (transferQuantity td) (transferCurrency td)
            , alignLeft . (" " ++) . show $ transferCurrency td
            , alignRight . fromMaybe " "
                $ showCurrencyQuantity <$> transferFeeQuantity td <*> transferFeeCurrency td
            , alignLeft . maybe " " ((" " ++) . show) $ transferFeeCurrency td
            , centeredString
                $ T.unpack (transferSourceExchange td)
                ++ " to "
                ++ T.unpack (transferDestinationExchange td)
            , group
            , comment
            , date
            ]
    where
        group =
            centeredString . T.unpack $ transactionGroup t
        comment =
            centeredString . T.unpack $ transactionComment t
        date =
            centeredString
                . formatTime defaultTimeLocale "%F %T"
                $ transactionDate t
        showCurrencyQuantity q c =
            if c == Currency "USD" then
                showQuantity 2 q
            else
                show q
        transferBuyAmount td =
            case (transferFeeCurrency td, transferFeeQuantity td) of
                (Just cur, Just q) ->
                    if cur == transferCurrency td then
                        transferQuantity td - q
                    else
                        transferQuantity td
                _ ->
                    transferQuantity td


-- | Center a String
centeredString :: String -> Widget n
centeredString s = if s == "" then str " " else C.center $ str s

-- | Align a String to the Right of it's Parent Widget.
alignRight :: String -> Widget n
alignRight s = if s == "" then str " " else padLeft Max $ str s

-- | Align a String to the Left of it's Parent Widget.
alignLeft :: String -> Widget n
alignLeft s = if s == "" then str " " else padRight Max $ str s
