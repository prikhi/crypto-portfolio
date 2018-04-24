{-# LANGUAGE LambdaCase #-}
module CoinTracking where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Csv ((.!))
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Time (defaultTimeLocale, formatTime)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Csv as Csv
import qualified Data.Vector as Vec

import Types


-- | Parse a Coin Tracking `Trade Table` CSV Export.
--
-- Deposits & Withdrawals with the same date, quantity, & currency are
-- merged into Transfers.
--
-- Trades with fees that are the same Currency as the buy Currency will
-- have the fee added to the buy amount, since CoinTracking assumes the fee
-- has been subtracted for calculations and we do not.
readTradeTableExport :: String -> IO [Transaction]
readTradeTableExport fileName = do
    csvData <- L.drop 1 . LC.dropWhile (/= '\n') <$> L.readFile fileName
    case Csv.decode Csv.NoHeader csvData of
        Left err ->
            putStrLn err >> return []
        Right v ->
            return
                . sortByDate
                . map fixFeeAmounts
                . mergeTransfers
                . map getTransaction
                $ Vec.toList v
    where
        sortByDate :: [Transaction] -> [Transaction]
        sortByDate =
            sortBy (flip compare `on` transactionDate)
        fixFeeAmounts :: Transaction -> Transaction
        fixFeeAmounts t = case transactionData t of
            Trade td ->
                case (,) <$> tradeFeeCurrency td <*> tradeFeeQuantity td of
                    Just (currency, amount) ->
                        if currency == tradeBuyCurrency td then
                            t
                                { transactionData =
                                    Trade td
                                        { tradeBuyQuantity =
                                            amount + tradeBuyQuantity td
                                        }
                                }
                        else
                            t
                    Nothing ->
                        t
            _ ->
                t
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



-- | Wrap the `Transaction` type so we can write custom Csv
-- decoding/encoding functions for CoinTracking.
newtype CTTransaction
    = CTTransaction
        { getTransaction :: Transaction
        } deriving (Show, Eq)


-- | Parse a Transaction from a CoinTracking.Info `Trade Table` Export
--
-- We have to use index-based parsing here because the export contains
-- 3 `"Cur."` columns
instance Csv.FromRecord CTTransaction where
    parseRecord v =
        if length v == 11 then do
            transactionType <- v .! 0
            date <- read <$> v .! 10
            group <- v .! 8
            comment <- v .! 9
            data_ <-
                if transactionType == ("Trade" :: String) then
                    Trade <$>
                        ( TradeData
                            <$> (readDecimalQuantity <$> v .! 1)
                            <*> (Currency <$> v .! 2)
                            <*> (readDecimalQuantity <$> v .! 3)
                            <*> (Currency <$> v .! 4)
                            <*> (fmap readDecimalQuantity <$> v .! 5)
                            <*> (fmap Currency <$> v .! 6)
                            <*> v .! 7
                        )
                else if isIncome transactionType then
                    Income <$>
                        ( IncomeData
                            <$> (readDecimalQuantity <$> v .! 1)
                            <*> (Currency <$> v .! 2)
                            <*> (fmap readDecimalQuantity <$> v .! 5)
                            <*> (fmap Currency <$> v .! 6)
                            <*> v .! 7
                        )
                else if isExpense transactionType then
                    Expense <$>
                        ( ExpenseData
                            <$> (readDecimalQuantity <$> v .! 3)
                            <*> (Currency <$> v .! 4)
                            <*> (fmap readDecimalQuantity <$> v .! 5)
                            <*> (fmap Currency <$> v .! 6)
                            <*> v .! 7
                        )
                else
                    mzero
            return . CTTransaction $ Transaction data_ date group comment
        else
            mzero
        where
            isIncome =
                (`elem` ["Income", "Mining", "Gift/Tip", "Deposit"])
            isExpense =
                (`elem` ["Withdrawal", "Spend", "Donation", "Gift", "Stolen/Hacked/Fraud", "Lost"])

-- | Generate a CoinTracking Trade Table Export row from a Transaction.
--
-- Trying to encode a `Transfer` will cause a runtime error. You should
-- ensure all transfers have been split into discrete income & expenses
-- since CoinTracking doesn't have a `Trasnfer` type.
--
-- This is used by the data generation script.
instance Csv.ToRecord CTTransaction where
    toRecord (CTTransaction t) = Csv.record
        [ Csv.toField transactionType
        , Csv.toField buyQuantity
        , Csv.toField buyCurrency
        , Csv.toField sellQuantity
        , Csv.toField sellCurrency
        , Csv.toField feeQuantity
        , Csv.toField feeCurrency
        , Csv.toField exchange
        , Csv.toField $ transactionGroup t
        , Csv.toField $ transactionComment t
        , Csv.toField $ formatTime defaultTimeLocale "%F %T" $ transactionDate t
        ]
        where
            ( transactionType, buyQuantity, buyCurrency, sellQuantity, sellCurrency, feeQuantity, feeCurrency, exchange ) =
                case transactionData t of
                    Trade td ->
                        ( "Trade" :: String
                        , show $ tradeBuyQuantity td
                        , show $ tradeBuyCurrency td
                        , show $ tradeSellQuantity td
                        , show $ tradeSellCurrency td
                        , show <$> tradeFeeQuantity td
                        , show <$> tradeFeeCurrency td
                        , tradeExchange td
                        )
                    Income d ->
                        ( "Income"
                        , show $ incomeQuantity d
                        , show $ incomeCurrency d
                        , ""
                        , ""
                        , show <$> incomeFeeQuantity d
                        , show <$> incomeFeeCurrency d
                        , incomeExchange d
                        )
                    Expense ed ->
                        ( "Spend"
                        , ""
                        , ""
                        , show $ expenseQuantity ed
                        , show $ expenseCurrency ed
                        , show <$> expenseFeeQuantity ed
                        , show <$> expenseFeeCurrency ed
                        , expenseExchange ed
                        )
                    Transfer _ ->
                        error "toRecord: Cannot Encode Transfers to Single CSV Row"
