{-|

Generate random Transactions & save them to the Trades CSV. You might use
this if you want to test out the app & don't have CoinTracking data, or if
you want to post a screenshot or video & don't want to show the world your
actual portfolio.

Eventually, maybe we should re-iterate investment cycle a couple times over
years, & be making income & expenses the whole time.

-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (MonadCatch, tryAny)
import Control.Monad (when, forM_, void, join, replicateM)
import Control.Monad.Reader (ReaderT, MonadReader, MonadIO, runReaderT, liftIO, asks)
import Data.IORef (IORef, newIORef, modifyIORef, readIORef, writeIORef)
import Data.List ((\\), delete)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Ratio ((%))
import Data.Time
    ( UTCTime(..), Day(..), NominalDiffTime, getCurrentTime, formatTime
    , defaultTimeLocale, addUTCTime
    )
import System.Directory (doesFileExist, renamePath)
import System.Random (Random)
import Test.QuickCheck (Gen, generate, elements, choose, frequency)

import qualified Data.ByteString.Lazy as LB
import qualified Data.Csv as Csv
import qualified Data.Map as M
import qualified Data.Text as T

import Types
import qualified Binance
import qualified GDAX


-- | The script has access to the global environment & runs in the IO
-- monad.
type Script a = ReaderT Env IO a


-- | The environment available to all `Script` functions, used to store
-- mutable references used throughout the script.
data Env =
    Env
        { _gdaxTotals :: IORef (M.Map Currency Quantity)
        , _binanceTotals :: IORef (M.Map Currency Quantity)
        , _trackedDate :: IORef UTCTime
        }

-- Pull values out of the Env.

class HasDate a where
    trackedDate :: a -> IORef UTCTime

instance HasDate Env where
    trackedDate = _trackedDate

class HasGDAXTotals a where
    gdaxTotals :: a -> IORef (M.Map Currency Quantity)

instance HasGDAXTotals Env where
    gdaxTotals = _gdaxTotals

class HasBinanceTotals a where
    binanceTotals :: a -> IORef (M.Map Currency Quantity)

instance HasBinanceTotals Env where
    binanceTotals = _binanceTotals


-- Manipulate the Env.

-- | Track a Date that can be incremented & fetched.
class Monad m => TrackDate m where
    setStartDate :: UTCTime -> m ()
    advanceBy :: NominalDiffTime -> m ()
    getTime :: m UTCTime

instance (HasDate env, MonadIO m) => TrackDate (ReaderT env m) where
    setStartDate d = do
        dRef <- asks trackedDate
        liftIO $ writeIORef dRef d
    advanceBy dt = do
        dRef <- asks trackedDate
        liftIO $ modifyIORef dRef (addUTCTime dt)
    getTime =
        asks trackedDate >>= liftIO . readIORef


-- | Fetch Prices from GDAX & Binance
class Monad m => GetPrice m where
    binancePrice :: Currency -> m Quantity
    gdaxPrice :: Currency -> m Quantity

-- | Used the tracked date to fetch the historical price.
instance (HasDate env, MonadIO m) => GetPrice (ReaderT env m) where
    binancePrice c = do
        time <- getTime
        (_, (_, price)) <- liftIO $ Binance.getHistoricalPrice c time
        return price
    gdaxPrice c = do
        time <- getTime
        (_, price) <- liftIO $ GDAX.getHistoricalPrice c time
        liftIO $ threadDelay 400000
        return price


-- | Build up the list of Transactions.
class Monad m => MonadTransaction m where
    addTransaction :: Transaction -> m ()

-- | Directly append the Transaction rows to the export file as we generate
-- them.
instance MonadIO m => MonadTransaction (ReaderT env m) where
    addTransaction transaction =
        liftIO
            . LB.appendFile "trade_table.csv"
            . Csv.encode
            $ splitTransfers [transaction]


-- | Track the Quantities on GDAX.
class Monad m => TrackGDAXQuantity m where
    gdaxAdd :: Currency -> Quantity -> m ()
    gdaxRemove :: Currency -> Quantity -> m ()
    getGDAXQuantity :: Currency -> m Quantity

instance (HasGDAXTotals env, MonadIO m) => TrackGDAXQuantity (ReaderT env m) where
    gdaxAdd =
        addToRefMap gdaxTotals
    gdaxRemove c q =
        gdaxAdd c (-q)
    getGDAXQuantity =
        getFromRefMap gdaxTotals

-- | Track the Quantities on Binance.
class Monad m => TrackBinanceQuantity m where
    binanceAdd :: Currency -> Quantity -> m ()
    binanceRemove :: Currency -> Quantity -> m ()
    getBinanceQuantity :: Currency -> m Quantity

instance (HasBinanceTotals env, MonadIO m) => TrackBinanceQuantity (ReaderT env m) where
    binanceAdd =
        addToRefMap binanceTotals
    binanceRemove c q =
        binanceAdd c (-q)
    getBinanceQuantity =
        getFromRefMap binanceTotals

-- | Update a Currency's Quantity in an IORef Map.
addToRefMap
    :: (MonadReader env m, MonadIO m)
    => (env -> IORef (M.Map Currency Quantity))
    -> Currency
    -> Quantity
    -> m ()
addToRefMap m c q =
    asks m >>= liftIO . flip modifyIORef (M.alter (Just . maybe q (+ q)) c)

-- | Retrieve a Currency's Quantity in an IORef Map.
getFromRefMap
    :: (MonadReader env m, MonadIO m)
    => (env -> IORef (M.Map Currency Quantity))
    -> Currency
    -> m Quantity
getFromRefMap m c =
    asks m >>= (\ts -> fromMaybe 0 . M.lookup c <$> liftIO (readIORef ts))


-- IO Sub-Systems

-- | Reads Files.
class Monad m => FileAccess m where
    fileExists :: String -> m Bool

instance FileAccess IO where
    fileExists =
        doesFileExist
instance MonadIO m => FileAccess (ReaderT env m) where
    fileExists =
        liftIO . fileExists

class Monad m => FileModification m where
    rename :: String -> String -> m ()

instance (FileAccess m, MonadIO m) => FileModification (ReaderT env m) where
    rename o n = do
        exists <- fileExists o
        when exists $
            liftIO . void . tryAny $ renamePath o n


-- | Writes Messages.
class Monad m => Logger m where
    logMessage :: T.Text -> m ()

instance MonadIO m => Logger (ReaderT env m) where
    logMessage =
        liftIO . putStrLn . T.unpack


-- | Generates Random Data.
class Monad m => RandomGen m where
    gen :: Gen a -> m a

instance MonadIO m => RandomGen (ReaderT env m) where
    gen =
        liftIO . generate


-- | Backup Any Existing Exports & Generate Some Fake Transactions.
--
-- This will generate a series of Transactions that:
--
--  * Invests $5-10k into BTC, LTC, & ETH over 4 weeks
--  * Transfers ~75% of the ETH to Binance
--  * Moves the remaining ETH, LTC, & BTC to wallets
--  * Buys BNB & 5-10 other Binance coins with that ETH over 4 weeks
--  * Makes some daily income & expense transactions
--  * Sells off some of the Binance coins for ETH.
--  * Transfers all the Binance ETH to GDAX.
--  * Sells 20% of the ETH.
--  * Transfers the remaining ETH to the wallet.
main :: IO ()
main =
    initial >>= void . runReaderT generateData
    where
        initial =
            Env
                <$> newIORef M.empty
                <*> newIORef M.empty
                <*> newIORef (UTCTime (ModifiedJulianDay 0) 0)

generateData :: Script ()
generateData = do
    currentTime <- liftIO getCurrentTime
    backupExistingExport currentTime :: Script ()
    pickStartDate currentTime
    initialGDAXInvestment
    transferEthToBinance
    transferGDAXToWallets
    advanceHour
    binanceCurrencies <- pickBinanceCurrencies
    initialBinanceInvestment binanceCurrencies
    logMessage "Initial Investments Completed."
    advanceWeek >> advanceWeek
    addIncomeAndExpenses
    logMessage "Income & Expense Generation Completed."
    advanceWeek >> advanceWeek
    takeProfits binanceCurrencies
    advanceHour
    transferEthToGDAX
    advanceHour
    sellEthereum
    logMessage "Sell Off Completed."
    where
        sellEthereum = do
            price <- gdaxPrice eth
            quantity <- getGDAXQuantity eth
            when (quantity > 0) $ do
                let buyAmount = price * quantity
                makeTrade 15 buyAmount (Currency "USD") quantity eth
                    Nothing Nothing "GDAX"
                gdaxRemove eth quantity
                logMessage
                    $ "Sold "
                    <> T.pack (show quantity)
                    <> " ETH for $"
                    <> T.pack (showQuantity 2 buyAmount)
                    <> "."
                advanceHour


-- | Backup any existing export to `trade_table-<date>-<time>.csv`.
backupExistingExport :: (FileAccess m, FileModification m, Logger m) => UTCTime -> m ()
backupExistingExport currentTime = do
    exists <- fileExists "trade_table.csv"
    when exists $ do
        let newPath =
                "trade_table-"
                    ++ formatTime defaultTimeLocale "%F-%T" currentTime
                    ++ ".csv"
        logMessage $ "Found Existing Trade Export, Backuping Up To: "
            <> T.pack newPath
        rename "trade_table.csv" newPath


-- | Pick a list of 5-10 Binance coins as well as BNB.
pickBinanceCurrencies :: (RandomGen m, Logger m) => m [Currency]
pickBinanceCurrencies = do
    altcoins <- gen $ do
        count <- choose (5, 10)
        coins <- fixedSubset count sampleBinanceCoins
        return $ Currency "BNB" : coins
    logMessage $ "Using the following Binance coins:\n"
        <> T.intercalate "\n" (map (\c -> "    * " <> T.pack (show c)) altcoins)
    return altcoins

-- | Pick a starting date of about 6 months ago.
pickStartDate :: (TrackDate m, RandomGen m, Logger m) => UTCTime -> m ()
pickStartDate currentTime = do
    daysAgo <- gen $ (-6 * 30) +- 20
    let startDate = addUTCTime (daysToDiffTime daysAgo) currentTime
    logMessage
        $ "Picked a Starting Date of "
        <> T.pack (formatTime defaultTimeLocale "%F" startDate)
        <> "."
    setStartDate startDate


-- | Purchase between $5-10k of Currencies on GDAX over ~4 Weeks.
initialGDAXInvestment
    :: (RandomGen m, TrackDate m, GetPrice m, MonadTransaction m, Logger m, TrackGDAXQuantity m)
    => m ()
initialGDAXInvestment = do
    currencyAndAmounts <- getUSDInvestments
    forM_ [1..4 :: Integer] $ \investmentNumber -> do
        forM_ currencyAndAmounts $ \(currency, usdToSpend) -> do
            usdPrice <- gdaxPrice currency
            let buyAmount = usdToSpend / usdPrice
            makeTrade investmentNumber buyAmount currency usdToSpend
                (Currency "USD") Nothing Nothing "GDAX"
            gdaxAdd currency buyAmount
            logMessage
                $ "Bought "
                <> T.pack (showQuantity 8 buyAmount)
                <> " "
                <> T.pack (show currency)
                <> " for $"
                <> T.pack (showQuantity 2 usdToSpend)
                <> "."
            advanceHour
        advanceWeek

-- | Pick the Amount of Funds to Invest in the GDAX Currencies, Splitting
-- $5-10k into 4 Purchases of BTC, ETH, & LTC.
getUSDInvestments :: (Logger m, RandomGen m) => m [(Currency, Quantity)]
getUSDInvestments = do
    (totalInvestment, splitInvestments) <- gen $ do
        initialUSDInvestment <- (% 1) <$> choose (5000, 10000 :: Integer)
        (btcRatio, ethRatio, ltcRatio) <- pickUSDInvestmentRatio
        return
            ( initialUSDInvestment
            , [ ( Currency "BTC", Quantity $ initialUSDInvestment * btcRatio / 4)
              , ( Currency "ETH", Quantity $ initialUSDInvestment * ethRatio / 4)
              , ( Currency "LTC", Quantity $ initialUSDInvestment * ltcRatio / 4)
              ]
            )
    logMessage $ "Starting with Initial Investment of $"
        <> T.pack (showRational 2 totalInvestment) <> "."
    return splitInvestments
    where
        -- | Pick the USD Investment Distribution.
        pickUSDInvestmentRatio :: Gen (Rational, Rational, Rational)
        pickUSDInvestmentRatio = do
            btcRatio <- (% 100) <$> 30 +- 5
            ltcRatio <- (% 100) <$> 5 +- 3
            let ethRatio = 1 - btcRatio - ltcRatio
            return (btcRatio, ethRatio, ltcRatio)


-- | Transfer ~75% of the ETH on GDAX to Binance.
transferEthToBinance
    :: ( RandomGen m, TrackGDAXQuantity m, TrackBinanceQuantity m, TrackDate m
       , MonadTransaction m, Logger m
       )
    => m ()
transferEthToBinance = do
    ethToBinance <- pickEthTransferAmount
    makeTransfer "GDAX" "Binance" Nothing Nothing eth ethToBinance
        "For Altcoin Purchases"
    gdaxRemove eth ethToBinance
    binanceAdd eth ethToBinance
    where
        pickEthTransferAmount :: (RandomGen m, TrackGDAXQuantity m) => m Quantity
        pickEthTransferAmount = do
            purchased <- getGDAXQuantity eth
            ratio <- (% 100) <$> gen (75 +- 10)
            return $ (* Quantity ratio) purchased


-- | Transfer All Currencies on GDAX to their Wallets.
transferGDAXToWallets
    :: (TrackGDAXQuantity m, TrackDate m, MonadTransaction m, Logger m)
    => m ()
transferGDAXToWallets =
    forM_ currencyAndWallets $ \(currency, wallet) -> do
        quantity <- getGDAXQuantity currency
        gdaxRemove currency quantity
        when (quantity > 0) $
            makeTransfer "GDAX" wallet Nothing Nothing
                currency quantity "Taking Coins Off Exchange"
    where
        currencyAndWallets =
            [ (Currency "BTC", "Bitcoin QT Wallet")
            , (eth, "Ethereum Mist Wallet")
            , (Currency "LTC", "Ledger Wallet")
            ]


-- | Use the ETH on Binance to Purchase BNB as well as the Given Coins.
--
-- Fees are in BNB & approximated to 0.1% of the trade's ETH value.
initialBinanceInvestment
    :: ( RandomGen m, TrackDate m, GetPrice m, MonadTransaction m, Logger m
       , MonadCatch m, TrackBinanceQuantity m
       )
    => [Currency]
    -> m ()
initialBinanceInvestment cs = do
    totalEth <- getBinanceQuantity eth
    remainingEth <- (totalEth -) <$> purchaseBNB totalEth
    advanceHour
    currenciesAndAmounts <- getBinanceInvestments remainingEth cs
    forM_ [5..8 :: Integer] $ \investmentNumber -> do
        forM_ currenciesAndAmounts $ \(currency, ethToSpend) -> void . tryAny $ do
            ethPrice <- binancePrice currency
            let buyAmount = ethToSpend / ethPrice
            fee <- calculateFee ethPrice buyAmount
            makeTrade investmentNumber buyAmount currency ethToSpend eth
                (Just fee) (Just bnb) "Binance"
            updateQuantities currency buyAmount ethToSpend fee
            logMessage
                $ "Bought "
                <> T.pack (show buyAmount)
                <> " "
                <> T.pack (show currency)
                <> " for "
                <> T.pack (show ethToSpend)
                <> " ETH."
            advanceHour
        advanceWeek
    where
        bnb :: Currency
        bnb =
            Currency "BNB"

        purchaseBNB
            :: ( RandomGen m, TrackDate m, GetPrice m, MonadTransaction m
               , Logger m, TrackBinanceQuantity m
               )
            => Quantity -> m Quantity
        purchaseBNB totalEth = do
            ethPrice <- binancePrice bnb
            let ethToSpend = 0.01 * totalEth
                buyAmount = ethToSpend / ethPrice
                feeAmount = 0.001 * buyAmount
            makeTrade 5 buyAmount bnb ethToSpend (Currency "ETH")
                (Just feeAmount) (Just bnb) "Binance"
            logMessage
                $ "Bought "
                <> T.pack (show buyAmount)
                <> " BNB for "
                <> T.pack (show ethToSpend)
                <> " ETH."
            updateQuantities bnb buyAmount ethToSpend feeAmount
            return ethToSpend

        updateQuantities
            :: TrackBinanceQuantity m
            => Currency -> Quantity -> Quantity -> Quantity -> m ()
        updateQuantities buyCurrency buyAmount ethSold feeAmount =
            binanceAdd buyCurrency buyAmount
                >> binanceRemove eth ethSold
                >> binanceRemove bnb feeAmount

        calculateFee
            :: (MonadTransaction m, GetPrice m)
            => Quantity -> Quantity -> m Quantity
        calculateFee currencyPrice buyAmount = do
            bnbPrice <- binancePrice bnb
            return $ buyAmount * currencyPrice * 0.001 / bnbPrice

-- | Calculate the Purchase Amounts for the Binance Investment.
--
-- Two or three currencies will be selected as large investments, splitting
-- 60% of of the available ETH between themselves. The other coins will
-- split the remaining 40%.
getBinanceInvestments :: RandomGen m => Quantity -> [Currency] -> m [(Currency, Quantity)]
getBinanceInvestments totalEth cs = do
    (deepCount, deepCoins) <- gen $ do
        c <- choose (2,3 :: Int)
        (c,) <$> fixedSubset c cs
    let shallowCount = length cs - deepCount
        deepInvestment = Quantity
            $ 0.6 * fromQuantity totalEth / toRational deepCount / 4
        shallowInvestment = Quantity
            $ 0.4 * fromQuantity totalEth / toRational shallowCount / 4
    return
        $ zip deepCoins (repeat deepInvestment)
        ++ zip (cs \\ deepCoins) (repeat shallowInvestment)

-- | Generate Some Income & Expense Transactions.
addIncomeAndExpenses :: (GetPrice m, RandomGen m, TrackDate m, MonadTransaction m, Logger m) => m ()
addIncomeAndExpenses = do
    addInitialIncome
    forM_ [1 .. 15 :: Integer] . const $ do
        ltcPrice <- gdaxPrice ltc
        join . gen $ do
            (constructor, details) <- frequency
                [ (3, pure (makeIncome, incomeDetails))
                , (1, pure (makeExpense, expenseDetails))
                ]
            (group, comment) <- elements details
            usdAmount <- Quantity . toRational <$> 10 +- (5 :: Integer)
            let ltcAmount = usdAmount / ltcPrice
                feeAmount = ltcAmount * 0.001
            return $ constructor ltc ltcAmount (Just ltc) (Just feeAmount)
                "Ledger Wallet" group comment
        replicateM 21 advanceHour
    where
        ltc :: Currency
        ltc =
            Currency "LTC"
        addInitialIncome
            :: (GetPrice m, RandomGen m, TrackDate m, MonadTransaction m, Logger m)
            => m ()
        addInitialIncome = do
            ltcPrice <- gdaxPrice ltc
            usdAmount <- fmap (Quantity . toRational) . gen $ 100 +- (25 :: Integer)
            let ltcAmount = usdAmount / ltcPrice
                feeAmount = 0.001 * ltcAmount
            makeIncome ltc ltcAmount (Just ltc) (Just feeAmount) "Ledger Wallet"
                "Craigslist" "Sold Narwhal Bacon"
        incomeDetails :: [(T.Text, T.Text)]
        incomeDetails =
            [ ( "Etsy", "Sold Plumbus" )
            , ( "Etsy", "Sold Replacement Fleebs" )
            , ( "LocalBitcoins", "Floob Repair" )
            , ( "LocalBitcoins", "Sold Seeds" )
            , ( "Website", "Sold L-Theanine" )
            , ( "Website", "Sold Oxiracetam" )
            ]
        expenseDetails :: [(T.Text, T.Text)]
        expenseDetails =
            [ ( "Dental Supplies", "Toothbrushes" )
            , ( "Dental Supplies", "Floss" )
            , ( "Dental Supplies", "Toothpaste" )
            , ( "Fun", "Movies" )
            , ( "Fun", "Games" )
            , ( "Food", "Groceries" )
            , ( "Food", "Restaurant" )
            , ( "Food", "Ice Cream" )
            , ( "Food", "Ginger Beer" )
            ]

-- | Sell 10% of Each Binance Coin For 5 Days.
takeProfits
    :: ( RandomGen m, TrackDate m, GetPrice m, TrackBinanceQuantity m
       , MonadTransaction m, Logger m, MonadCatch m
       )
    => [Currency] -> m ()
takeProfits cs =
    forM_ [9 .. 14 :: Integer] $ \investmentNumber -> do
        forM_ cs $ \currency -> void . tryAny $ do
            quantity <- getBinanceQuantity currency
            when (quantity > 0) $ do
                sellPercent <- (% 100) <$> gen (10 +- 2 :: Gen Integer)
                ethPrice <- binancePrice currency
                let sellAmount = quantity * Quantity sellPercent
                    buyAmount = sellAmount * ethPrice
                    feeAmount = buyAmount * 0.001
                makeTrade investmentNumber  buyAmount eth sellAmount currency
                    (Just feeAmount) (Just $ Currency "BNB") "Binance"
                logMessage
                    $ "Bought "
                    <> T.pack (show buyAmount)
                    <> " "
                    <> T.pack (show eth)
                    <> " for "
                    <> T.pack (show sellAmount)
                    <> " "
                    <> T.pack (show currency)
                    <> "."
                binanceAdd eth buyAmount
                binanceRemove currency sellAmount
                binanceRemove (Currency "BNB") feeAmount
                advanceHour
        replicateM 16 advanceHour

transferEthToGDAX
    :: ( TrackBinanceQuantity m, TrackGDAXQuantity m, TrackDate m
       , MonadTransaction m, Logger m, GetPrice m
       )
    => m ()
transferEthToGDAX = do
    availableEth <- getBinanceQuantity eth
    when (availableEth > 0) $ do
        ethPrice <- gdaxPrice eth
        let feeAmount = 10 / ethPrice
        makeTransfer "Binance" "GDAX" (Just feeAmount) (Just eth)
            eth availableEth "From Altcoin Sales"
        binanceRemove eth availableEth
        gdaxAdd eth $ availableEth - feeAmount


-- UTILS

-- | Split any Transfers into an Income & Expense.
splitTransfers :: [Transaction] -> [Transaction]
splitTransfers ts = case ts of
    [] ->
        []
    t : rest ->
        case transactionData t of
            Transfer td ->
                income t td : expense t td : splitTransfers rest
            _ ->
                t : splitTransfers rest
    where
        income :: Transaction -> TransferData -> Transaction
        income t td =
            t
                { transactionData = Income IncomeData
                    { incomeQuantity =
                        case (,) <$> transferFeeQuantity td <*> transferFeeCurrency td of
                            Just (feeQ, feeC) ->
                                if feeC == transferCurrency td then
                                    transferQuantity td - feeQ
                                else
                                    transferQuantity td
                            Nothing ->
                                transferQuantity td
                    , incomeCurrency =
                        transferCurrency td
                    , incomeFeeQuantity = Nothing
                    , incomeFeeCurrency = Nothing
                    , incomeExchange = transferDestinationExchange td
                    }
                }
        expense :: Transaction -> TransferData -> Transaction
        expense t td =
            t
                { transactionData = Expense ExpenseData
                    { expenseQuantity = transferQuantity td
                    , expenseCurrency = transferCurrency td
                    , expenseFeeQuantity = transferFeeQuantity td
                    , expenseFeeCurrency = transferFeeCurrency td
                    , expenseExchange = transferSourceExchange td
                    }
                }

-- | Construct & Add a Trade to the Transaction List.
makeTrade
    :: (MonadTransaction m, TrackDate m)
    => Integer
    -> Quantity
    -> Currency
    -> Quantity
    -> Currency
    -> Maybe Quantity
    -> Maybe Currency
    -> T.Text
    -> m ()
makeTrade investmentNumber buyAmount buyCurrency sellAmount sellCurrency maybeFee maybeCurrency exchange = do
    time <- getTime
    addTransaction
        Transaction
            { transactionDate = time
            , transactionGroup = "Investment #" <> T.pack (show investmentNumber)
            , transactionComment = ""
            , transactionData =
                Trade TradeData
                    { tradeBuyQuantity = buyAmount
                    , tradeBuyCurrency = buyCurrency
                    , tradeSellQuantity = sellAmount
                    , tradeSellCurrency = sellCurrency
                    , tradeFeeQuantity = maybeFee
                    , tradeFeeCurrency = maybeCurrency
                    , tradeExchange = exchange
                    }
            }

-- | Transfer Some Amount of Currency Between Two Exchanges.
makeTransfer
    :: (TrackDate m, MonadTransaction m, Logger m)
    => T.Text
    -> T.Text
    -> Maybe Quantity
    -> Maybe Currency
    -> Currency
    -> Quantity
    -> T.Text
    -> m ()
makeTransfer sourceExchange destExchange maybeFee feeCurrency currency quantity comment = do
    logMessage
        $ "Transfering "
        <> T.pack (show quantity)
        <> " "
        <> T.pack (show currency)
        <> " From "
        <> sourceExchange
        <> " to "
        <> destExchange
        <> "."
    time <- getTime
    addTransaction Transaction
        { transactionDate = time
        , transactionGroup = ""
        , transactionComment = comment
        , transactionData = Transfer TransferData
            { transferQuantity = quantity
            , transferCurrency = currency
            , transferFeeQuantity = maybeFee
            , transferFeeCurrency = feeCurrency
            , transferSourceExchange = sourceExchange
            , transferDestinationExchange = destExchange
            }
        }


-- | Construct and Income Transaction & Add it to the Transaction List.
makeIncome
    :: (MonadTransaction m, TrackDate m, Logger m)
    => Currency -> Quantity -> Maybe Currency -> Maybe Quantity -> T.Text -> T.Text -> T.Text -> m ()
makeIncome c q feeC feeQ exch grp cmnt = do
    logMessage
        $ "Made "
        <> T.pack (show q)
        <> " "
        <> T.pack (show c)
        <> " from "
        <> grp
        <> " - "
        <> cmnt
    time <- getTime
    addTransaction Transaction
        { transactionDate = time
        , transactionGroup = grp
        , transactionComment = cmnt
        , transactionData = Income IncomeData
            { incomeQuantity = q
            , incomeCurrency = c
            , incomeFeeQuantity = feeQ
            , incomeFeeCurrency = feeC
            , incomeExchange = exch
            }
        }

-- | Construct and Expense Transaction & Add it to the Transaction List.
makeExpense
    :: (MonadTransaction m, TrackDate m, Logger m)
    => Currency -> Quantity -> Maybe  Currency -> Maybe Quantity -> T.Text -> T.Text -> T.Text -> m ()
makeExpense c q feeC feeQ exch grp cmnt = do
    logMessage
        $ "Spent "
        <> T.pack (show q)
        <> " "
        <> T.pack (show c)
        <> " on "
        <> grp
        <> " - "
        <> cmnt
    time <- getTime
    addTransaction Transaction
        { transactionDate = time
        , transactionGroup = grp
        , transactionComment = cmnt
        , transactionData = Expense ExpenseData
            { expenseQuantity = q
            , expenseCurrency = c
            , expenseFeeQuantity = feeQ
            , expenseFeeCurrency = feeC
            , expenseExchange = exch
            }
        }

-- | Advance the Tracked Date by an Hour.
advanceHour :: (RandomGen m, TrackDate m) => m()
advanceHour = do
    minutes <- fromInteger <$> gen (75 +- 15)
    advanceBy $ minutes * 60

-- | Advance the Tracked Date by a Week.
advanceWeek :: (RandomGen m, TrackDate m) => m ()
advanceWeek = do
    approxWeek <- daysToDiffTime <$> gen (7 +- 2)
    timeFuzz <- (\h -> fromInteger $ h * 60 * 60) <$> gen (12 +- 12)
    advanceBy $ approxWeek + timeFuzz

-- | Turn a number of days into a differential time.
daysToDiffTime :: Integer -> NominalDiffTime
daysToDiffTime d =
    fromInteger $ d * 24 * 60 * 60

-- | Generate a fixed-length subset of the given elements.
fixedSubset :: (Show a, Eq a) => Int -> [a] -> Gen [a]
fixedSubset n items
    | n > length items =
        error $ "fixedSubset: got longer subset than options!\n\t" ++ show items
    | n == 0 =
        return []
    | otherwise = do
        x <- elements items
        rest <- fixedSubset (n - 1) (delete x items)
        return $ x : rest

(+-) :: (Num a, Random a) => a -> a -> Gen a
x +- y = do
    fuzz <- choose (-y, y)
    return $ x + fuzz


-- | A list of Binance coins with ETH markets to choose from.
sampleBinanceCoins :: [Currency]
sampleBinanceCoins =
    [ Currency "ADA"
    , Currency "DNT"
    , Currency "EOS"
    , Currency "FUN"
    , Currency "LSK"
    , Currency "NANO"
    , Currency "NEO"
    , Currency "OMG"
    , Currency "ONT"
    , Currency "TRX"
    , Currency "SYS"
    , Currency "VEN"
    , Currency "WTC"
    , Currency "XLM"
    , Currency "XMR"
    , Currency "XRP"
    ]
