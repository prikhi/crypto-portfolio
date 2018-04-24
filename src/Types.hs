{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import Data.Binary (Binary)
import Data.Ratio (numerator, denominator)
import Data.Scientific (Scientific)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

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
        } deriving (Show, Eq)

-- | TODO: Change all `Exchange` fields to `Account`? To make more general for wallets
-- TODO: Change fee's from 2 maybes into single maybe tuple.
data TransactionData
    = Trade TradeData
    | Income IncomeData
    | Expense ExpenseData
    | Transfer TransferData
    deriving (Show, Eq)

data TradeData
    = TradeData
        { tradeBuyQuantity :: Quantity
        , tradeBuyCurrency :: Currency
        , tradeSellQuantity :: Quantity
        , tradeSellCurrency :: Currency
        , tradeFeeQuantity :: Maybe Quantity
        , tradeFeeCurrency :: Maybe Currency
        , tradeExchange :: T.Text
        } deriving (Show, Eq)

data IncomeData
    = IncomeData
        { incomeQuantity :: Quantity
        , incomeCurrency :: Currency
        , incomeFeeQuantity :: Maybe Quantity
        , incomeFeeCurrency :: Maybe Currency
        , incomeExchange :: T.Text
        } deriving (Show, Eq)

data ExpenseData
    = ExpenseData
        { expenseQuantity :: Quantity
        , expenseCurrency :: Currency
        , expenseFeeQuantity :: Maybe Quantity
        , expenseFeeCurrency :: Maybe Currency
        , expenseExchange :: T.Text
        } deriving (Show, Eq)

data TransferData
    = TransferData
        { transferQuantity :: Quantity
        , transferCurrency :: Currency
        , transferFeeQuantity :: Maybe Quantity
        , transferFeeCurrency :: Maybe Currency
        , transferSourceExchange :: T.Text
        , transferDestinationExchange :: T.Text
        } deriving (Show, Eq)
