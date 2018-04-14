{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Binance
    ( getPair
    , getHistoricalPrice
    , connect
    ) where

import Control.Lens ((^.), (.~), (&))
import Control.Monad (forever)
import Data.Aeson ((.:), FromJSON(..), eitherDecode, withObject, Value(Object))
import Data.Aeson.Lens (nth, _String)
import Data.Char (toLower)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.WebSockets (receiveData)
import Network.Wreq (getWith, defaults, param, responseBody)
import Wuss (runSecureClient)

import qualified Data.Text as T

import Types


-- | Currencies that have no ETH market.
--
-- TODO: Fill out this list, or pull tickers from binance & generate on start.
-- Maybe load from cache file & async-ly update occasionally
-- See exchangeInfo API route.
--
-- TODO: More efficient as a Set or (Currency -> Bool) if this list gets
-- long.
btcOnlyCurrencies :: [Currency]
btcOnlyCurrencies =
    [ Currency "GAS"
    ]

-- | Get the Pair Currency to Use for USD Conversions. Usually ETH, but
-- falls back to BTC if there is no XXXETH market.
--
-- TODO: There could be an IO version of this that checks the available
-- markets using an API call.
getPair :: Currency -> Currency
getPair c =
    if c `elem` btcOnlyCurrencies then
        Currency "BTC"
    else
        eth

-- | Grab the Pair Price for a Currency at the given time.
--
-- The returned price is the open price of the 1m Kline interval at the
-- specified time.
getHistoricalPrice :: Currency -> UTCTime -> IO ((Currency, UTCTime), (Currency, Quantity))
getHistoricalPrice c@(Currency symbol) time = do
    let pair =
            show $ getPair c
        opts =
            defaults
                & param "symbol" .~ [T.pack $ symbol ++ pair]
                & param "interval" .~ ["1m"]
                & param "limit" .~ ["1"]
                & param "startTime" .~ [T.pack $ show startTime]
                & param "endTime" .~ [T.pack $ show endTime]
    r <- getWith opts "https://api.binance.com/api/v1/klines"
    let openPrice = r ^. responseBody . nth 0 . nth 1 . _String
    return
        ( (Currency symbol, time)
        , (Currency pair, readDecimalQuantity $ T.unpack openPrice)
        )
    where
        -- | Convert the time into millisecond POSIX time.
        startTime :: Integer
        startTime =
            round $ utcTimeToPOSIXSeconds time * 1000
        endTime :: Integer
        endTime =
            startTime + (60 * 1000)


-- | Connect to Binance, Subscribe to an 24hr Ticker Stream & Run the
-- Specified Action with the Price Everytime we Receive a Message.
--
-- TODO: Pass currency list & use combined streams.
-- TODO: Eventually, some way to update the currency list from Brick.
connect :: Currency -> (String -> IO ()) -> IO ()
connect c@(Currency symbol) action = run $ \connection ->
    forever $ do
        msg <- eitherDecode <$> receiveData connection
        case msg of
            Right Ticker { price } ->
                action price
            _ ->
                return ()
    where
        run =
            runSecureClient "stream.binance.com" 9443 ("/ws/" ++ streamName)
        streamName =
            map toLower $ symbol ++ show (getPair c) ++ "@ticker"



-- | The only response we care about are aggregate trades.
data BinanceResponse
    = Ticker
        { price :: String
        }
    | Unimplemented Value
    deriving (Show)


instance FromJSON BinanceResponse where
    parseJSON = withObject "BinanceResponse" $ \v -> do
        evType <- v .: "e"
        if evType == ("24hrTicker" :: T.Text) then
            Ticker <$> v .: "c"
        else
            return . Unimplemented $ Object v
