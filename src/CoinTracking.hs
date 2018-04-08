{-# LANGUAGE LambdaCase #-}
module CoinTracking where

import Control.Applicative ((<|>))
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Csv as Csv
import qualified Data.Vector as Vec

import Types

-- | Parse a Coin Tracking `Trade Table` CSV Export.
readTradeTableExport :: String -> IO [Transaction]
readTradeTableExport fileName = do
    csvData <- L.drop 1 . LC.dropWhile (/= '\n') <$> L.readFile fileName
    case Csv.decode Csv.NoHeader csvData of
        Left err ->
            putStrLn err >> return []
        Right v ->
            return . sortByDate . mergeTransfers $ Vec.toList v
    where
        sortByDate :: [Transaction] -> [Transaction]
        sortByDate =
            sortBy (flip compare `on` transactionDate)
        mergeTransfers :: [Transaction] -> [Transaction]
        mergeTransfers = \case
            [] ->
                []
            [transaction] ->
                [transaction]
            t1 : t2 : rest ->
                case (transactionData t1, transactionData t2) of
                    (Income incomeData, Expense expenseData) ->
                        if isTransfer incomeData expenseData && sameDate t1 t2 then
                            makeTransfer t1 incomeData expenseData : mergeTransfers rest
                        else
                            t1 : mergeTransfers (t2 : rest)
                    (Expense expenseData, Income incomeData) ->
                        if isTransfer incomeData expenseData && sameDate t1 t2 then
                            makeTransfer t1 incomeData expenseData : mergeTransfers rest
                        else
                            t1 : mergeTransfers (t2 : rest)
                    _ ->
                        t1 : mergeTransfers (t2 : rest)
        isTransfer :: IncomeData -> ExpenseData -> Bool
        isTransfer incomeData expenseData =
            (incomeQuantity incomeData == expenseQuantity expenseData
                || incomeQuantity incomeData ==
                        (expenseQuantity expenseData
                            - fromMaybe 0 (expenseFeeQuantity expenseData))
                || expenseQuantity expenseData ==
                        (incomeQuantity incomeData
                            + fromMaybe 0 (incomeFeeQuantity incomeData))
            )
                && incomeCurrency incomeData == expenseCurrency expenseData
                && sameCurrencyOrNothing (incomeFeeCurrency incomeData) (expenseFeeCurrency expenseData)
        sameCurrencyOrNothing :: Maybe Currency -> Maybe Currency -> Bool
        sameCurrencyOrNothing mC1 mC2 =
            fromMaybe True $ (==) <$> mC1 <*> mC2
        sameDate :: Transaction -> Transaction -> Bool
        sameDate t1 t2 =
            transactionDate t1 == transactionDate t2
        makeTransfer :: Transaction -> IncomeData -> ExpenseData -> Transaction
        makeTransfer baseTransaction incomeData expenseData =
            baseTransaction { transactionData =
                Transfer TransferData
                    { transferQuantity =
                        incomeQuantity incomeData
                    , transferCurrency =
                        incomeCurrency incomeData
                    , transferFeeQuantity =
                        addFees (incomeFeeQuantity incomeData)
                            (expenseFeeQuantity expenseData)
                    , transferFeeCurrency =
                        expenseFeeCurrency expenseData <|> incomeFeeCurrency incomeData
                    , transferSourceExchange =
                        expenseExchange expenseData
                    , transferDestinationExchange =
                        incomeExchange incomeData
                    }
            }
        addFees :: Maybe Quantity -> Maybe Quantity -> Maybe Quantity
        addFees mQ1 mQ2 =
            case (mQ1, mQ2) of
                (Just q1, Just q2) ->
                    Just $ q1 + q2
                (Just _, Nothing) ->
                    mQ1
                (Nothing, Just _) ->
                    mQ2
                (Nothing, Nothing) ->
                    Nothing
