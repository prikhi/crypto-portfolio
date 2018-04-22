{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Control.Monad (mzero)
import Data.Binary (Binary)
import Data.Csv ((.!))
import Data.Ratio (numerator, denominator)
import Data.Scientific (Scientific)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import GHC.Generics (Generic)

import qualified Data.Csv as Csv
import qualified Data.Text as T

-- TODO: Save all this in a Persistent database

-- | Represents the Unique Identifiers of Widgets Used in the Application.
data AppWidget
    = EthereumGainsTable
    | USDGainsTable
    | TradeListTable
    deriving (Eq, Ord, Show)


-- FIELDS

-- | An Amount of a Coin that has been Bought/Sold, or a Per-Unit Price.
-- TODO: Make Integer Instead of Rational, Need to Figure Out Atomic Units
newtype Quantity
    = Quantity
        { fromQuantity :: Rational
        } deriving (Eq, Ord, Num, Fractional, Generic)

instance Binary Quantity

-- | Show 8 Decimal Places by Default.
instance Show Quantity where
    show = showQuantity 8

-- | Read a `Quantity` from a `Scientific`-formatted String
readDecimalQuantity :: String -> Quantity
readDecimalQuantity =
    Quantity . toRational . (read :: String -> Scientific)

-- | Render a `Quantity` with a Fixed Number of Decimal Places
showQuantity :: Int -> Quantity -> String
showQuantity decimalPlaces (Quantity rat) =
    showRational decimalPlaces rat

-- | Render a `Rational` with a Fixed Number of Decimal Places
showRational :: Int -> Rational -> String
showRational decimalPlaces rat =
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
    deriving (Ord, Eq, Generic)

instance Binary Currency

-- | Currencies are Represented by their Ticker Symbol
instance Show Currency where
    show = toSymbol

-- | The `Currency` Representing Ethereum.
eth :: Currency
eth =
    Currency "ETH"

-- TODO: Add `usd` & `btc` currencies as well, maybe patterns for case matching



-- TRANSACTIONS

data Transaction
    = Transaction
        { transactionData :: TransactionData
        , transactionDate :: UTCTime
        , transactionGroup :: T.Text
        , transactionComment :: T.Text
        } deriving (Show)

-- | TODO: Change all `Exchange` fields to `Account`? To make more general for wallets
-- TODO: Change fee's from 2 maybes into single maybe tuple.
data TransactionData
    = Trade TradeData
    | Income IncomeData
    | Expense ExpenseData
    | Transfer TransferData
    deriving (Show)

data TradeData
    = TradeData
        { tradeBuyQuantity :: Quantity
        , tradeBuyCurrency :: Currency
        , tradeSellQuantity :: Quantity
        , tradeSellCurrency :: Currency
        , tradeFeeQuantity :: Maybe Quantity
        , tradeFeeCurrency :: Maybe Currency
        , tradeExchange :: T.Text
        } deriving (Show)

data IncomeData
    = IncomeData
        { incomeQuantity :: Quantity
        , incomeCurrency :: Currency
        , incomeFeeQuantity :: Maybe Quantity
        , incomeFeeCurrency :: Maybe Currency
        , incomeExchange :: T.Text
        } deriving (Show)

data ExpenseData
    = ExpenseData
        { expenseQuantity :: Quantity
        , expenseCurrency :: Currency
        , expenseFeeQuantity :: Maybe Quantity
        , expenseFeeCurrency :: Maybe Currency
        , expenseExchange :: T.Text
        } deriving (Show)

data TransferData
    = TransferData
        { transferQuantity :: Quantity
        , transferCurrency :: Currency
        , transferFeeQuantity :: Maybe Quantity
        , transferFeeCurrency :: Maybe Currency
        , transferSourceExchange :: T.Text
        , transferDestinationExchange :: T.Text
        } deriving (Show)


-- | Parse a Transaction from a CoinTracking.Info `Trade Table` Export
--
-- We have to use index-based parsing here because the export contains
-- 3 `"Cur."` columns
--
-- TODO: Make a `CTTransaction` newtype in `CoinTracking` module & move these
-- instances over there...
instance Csv.FromRecord Transaction where
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
            return $ Transaction data_ date group comment
        else
            mzero
        where
            isIncome =
                (`elem` ["Income", "Mining", "Gift/Tip", "Deposit"])
            isExpense =
                (`elem` ["Withdrawal", "Spend", "Donation", "Gift", "Stolen/Hacked/Fraud", "Lost"])
-- | Generate a CoinTracking Trade Table Export row from a Transaction.
--
-- This is used by the data generation script.
instance Csv.ToRecord Transaction where
    toRecord t = Csv.record
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
