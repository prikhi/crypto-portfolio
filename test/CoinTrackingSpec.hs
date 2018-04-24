{- |

Test the CoinTracking module.

-}
module CoinTrackingSpec where

import Control.Exception (evaluate)
import Control.Monad (replicateM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime(..), Day(..))
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.Csv as Csv
import qualified Data.Text as T
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import CoinTracking
import Generators
import Types

spec :: Spec
spec = parallel $
    describe "CSV instances" $ do
        specify "toRecord followed by parseRecord is idempotent" $
            require $ property $ do
                transaction <- forAll genTransaction
                tripping transaction Csv.toRecord (Csv.runParser . Csv.parseRecord)
        specify "toRecord throws an error when passed a transfer" $ do
            transfers <- replicateM 20 . liftIO $ Gen.sample
                $ Transfer <$> genTransfer >>= transactionWithData
            forM_ transfers $ \transfer -> evaluate
                (Csv.runParser . (Csv.parseRecord :: Csv.Record -> Csv.Parser CTTransaction)
                        $ Csv.toRecord transfer)
                `shouldThrow`
                    errorCall "toRecord: Cannot Encode Transfers to Single CSV Row"


-- | Generate a Trade, Expense, or Income. CoinTracking has no Transfers.
genTransaction :: MonadGen m => m CTTransaction
genTransaction =
    transactionWithData =<< Gen.choice
        [ Trade <$> genTrade
        , Expense <$> genExpense
        , Income <$> genIncome
        ]

-- | Generate a Transaction using the given TransactionData.
transactionWithData :: MonadGen m => TransactionData -> m CTTransaction
transactionWithData data_ = do
    date <- genTime
    group <- Gen.text (Range.constant 0 30) Gen.alphaNum
    comment <- Gen.text (Range.constant 0 30) Gen.alphaNum
    return . CTTransaction $ Transaction
        { transactionData = data_
        , transactionDate = date
        , transactionGroup = group
        , transactionComment = comment
        }

-- | Generate Trade Data.
genTrade :: MonadGen m => m TradeData
genTrade = do
    (buyQuantity, sellQuantity) <- double positiveQuantity
    (buyCurrency, sellCurrency) <- double genCurrency
    maybeFee <- Gen.maybe $ (,) <$> positiveQuantity <*> genCurrency
    exchange <- genExchange
    return TradeData
        { tradeBuyQuantity = truncateQuantity buyQuantity
        , tradeBuyCurrency = buyCurrency
        , tradeSellQuantity = truncateQuantity sellQuantity
        , tradeSellCurrency = sellCurrency
        , tradeFeeQuantity = truncateQuantity . fst <$> maybeFee
        , tradeFeeCurrency = snd <$> maybeFee
        , tradeExchange = exchange
        }

-- | Generate a Transfer between Exchanges.
genTransfer :: MonadGen m => m TransferData
genTransfer = do
    destination <- genExchange
    (\f -> f destination) <$> singleCurrencySingleExchange TransferData

-- | Generate an Expense.
genExpense :: MonadGen m => m ExpenseData
genExpense =
    singleCurrencySingleExchange ExpenseData

-- | Generate some Income.
genIncome :: MonadGen m => m IncomeData
genIncome =
    singleCurrencySingleExchange IncomeData

-- | Generate data for a constructor taking a single currency, fee, & exchange.
singleCurrencySingleExchange
    :: MonadGen m
    => (Quantity -> Currency -> Maybe Quantity -> Maybe Currency -> T.Text -> a)
    -> m a
singleCurrencySingleExchange constructor = do
    quantity <- truncateQuantity <$> positiveQuantity
    currency <- genCurrency
    maybeFee <- Gen.maybe $ (,) <$> positiveQuantity <*> genCurrency
    exchange <- genExchange
    return $ constructor quantity currency (truncateQuantity . fst <$> maybeFee)
        (snd <$> maybeFee) exchange

-- | Turn generated repeating decimals into fixed decimals. This is
-- necessary because generators will make quantities like (1/3) but only
-- quantities with a fixed number of decimal places can exist in the
-- exports.
truncateQuantity :: Quantity -> Quantity
truncateQuantity =
    readDecimalQuantity . show

-- | Run a generator twice and return both results
double :: MonadGen m => m a -> m (a, a)
double ma = do
    a <- ma
    b <- ma
    return (a, b)

genCurrency :: MonadGen m => m Currency
genCurrency =
    Currency <$> Gen.string (Range.constant 3 4) Gen.upper

genExchange :: MonadGen m => m T.Text
genExchange =
    Gen.text (Range.constant 1 10) Gen.alphaNum

genTime :: MonadGen m => m UTCTime
genTime = do
    day <- ModifiedJulianDay . fromIntegral <$> Gen.int (Range.constant 0 maxBound)
    time <- fromRational . toRational <$> Gen.int (Range.constant 0 86400)
    return $ UTCTime day time
