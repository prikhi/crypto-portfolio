{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module TradeList
    ( view
    ) where

import Brick
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Time.Format (formatTime, defaultTimeLocale)

import qualified Brick.Widgets.Border as B
import qualified Data.Text as T

import Table
import Types


-- RENDER

view :: [Transaction] -> [Widget AppWidget]
view transactions =
    [ vBox
        [ B.hBorderWithLabel (str " Trade List ")
        , B.border
            . padLeftRight 1
            $ table tableConfig transactions
        ]
    ]


tableConfig :: TableConfig Transaction AppWidget
tableConfig =
    TableConfig
        { columns = tableColumns
        , showRowDividers = True
        , footerRows = []
        , name = TradeListTable
        }


tableColumns :: [Column Transaction]
tableColumns =
    [ textColumn
        { headerName = "Type"
        , columnWeight = 5
        , dataSelector = typeSelector
        }
    , column
        { headerName = "Buy"
        , dataSelector = buyQuantity
        }
    , column
        { headerName = "Sell"
        , dataSelector = sellQuantity
        }
    , column
        { headerName = "Fee"
        , dataSelector = feeQuantity
        }
    , textColumn
        { headerName = "Exchange"
        , dataSelector = exchange
        , columnWeight = 10
        }
    , textColumn
        { headerName = "Group"
        , dataSelector = T.unpack . transactionGroup
        }
    , textColumn
        { headerName = "Comment"
        , dataSelector = T.unpack . transactionComment
        }
    , textColumn
        { headerName = "Date"
        , dataSelector = formatTime defaultTimeLocale "%F %T" . transactionDate
        }
    ]
    where
        typeSelector = withData $ \case
            Trade _ ->
                "Trade"
            Income _ ->
                "Income"
            Expense _ ->
                "Expense"
            Transfer _ ->
                "Transfer"
        buyQuantity = withData $ \case
            Trade td ->
                showCurrencyQuantity (tradeBuyQuantity td) (tradeBuyCurrency td)
            Income d ->
                showCurrencyQuantity (incomeQuantity d) (incomeCurrency d)
            Expense _ ->
                ""
            Transfer td ->
                showCurrencyQuantity (transferBuyAmount td) (transferCurrency td)
        sellQuantity = withData $ \case
            Trade td ->
                showCurrencyQuantity (tradeSellQuantity td) (tradeSellCurrency td)
            Income _ ->
                ""
            Expense ed ->
                showCurrencyQuantity (expenseQuantity ed) (expenseCurrency ed)
            Transfer td ->
                showCurrencyQuantity (transferQuantity td) (transferCurrency td)
        feeQuantity = withData $ fromMaybe "" . \case
            Trade td ->
                showCurrencyQuantity <$> tradeFeeQuantity td <*> tradeFeeCurrency td
            Income d ->
                showCurrencyQuantity <$> incomeFeeQuantity d <*> incomeFeeCurrency d
            Expense ed ->
                showCurrencyQuantity <$> expenseFeeQuantity ed <*> expenseFeeCurrency ed
            Transfer td ->
                showCurrencyQuantity <$> transferFeeQuantity td <*> transferFeeCurrency td
        exchange = withData $ T.unpack . \case
            Trade td ->
                tradeExchange td
            Income d ->
                incomeExchange d
            Expense ed ->
                expenseExchange ed
            Transfer td ->
                transferSourceExchange td
                    <> " to "
                    <> transferDestinationExchange td


        withData f =
            f . transactionData

        showCurrencyQuantity q c = (\qs -> qs ++ " " ++ show c) $
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
        textColumn =
            column
                { headerAlign = Alignment VMiddle HCenter
                , dataAlign = Alignment VMiddle HCenter
                , columnWeight = 10
                }
        column =
            Column
                { headerName = ""
                , headerAlign = Alignment VMiddle HRight
                , dataAlign = Alignment VMiddle HRight
                , columnWeight = 15
                , dataSelector = const ""
                }
