{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GDAX
    ( currencies
    , getHistoricalPrice
    , connect
    ) where

import Control.Lens ((&), (.~), (^?))
import Control.Monad (forever)
import Data.Aeson
    ( ToJSON(..), FromJSON(..), (.=), (.:), encode, eitherDecode, object
    , withObject, Value(Object)
    )
import Data.Aeson.Lens (nth, _Number)
import Data.Time (UTCTime, addUTCTime)
import Data.Time.ISO8601 (formatISO8601)
import Network.WebSockets (receiveData, sendTextData)
import Network.Wreq (getWith, defaults, param, responseBody)
import Wuss (runSecureClient)

import qualified Data.Text as T

import Types

currencies :: [Currency]
currencies =
    [ Currency "BTC"
    , Currency "ETH"
    , Currency "LTC"
    , Currency "BCH"
    ]

-- | Get the Historical USD Price for a GDAX Currency.
--
-- Returns the Open Price for the 15 minute candle starting at the given
-- time. If no data is found, check for a candle 15 minutes in
-- the past until we find a price.
getHistoricalPrice :: Currency -> UTCTime -> IO ((Currency, UTCTime), Quantity)
getHistoricalPrice c@(Currency symbol) time = do
    let opts =
            defaults
                & param "granularity" .~ ["900"]
                & param "start" .~ [startTime]
                & param "end" .~ [endTime]
    r <- getWith opts $ "https://api.gdax.com/products/" ++ productId ++ "/candles"
    let openPrice = r ^? responseBody . nth 0 . nth 3 . _Number
    case openPrice of
        Nothing ->
            getHistoricalPrice c $ addUTCTime (-1 * 15 * 60) time
        Just price ->
            return
                ( (Currency symbol, time)
                , Quantity $ toRational price
                )
    where
        productId =
            symbol ++ "-USD"
        startTime =
            T.pack $ formatISO8601 time
        endTime =
            T.pack . formatISO8601 $ addUTCTime (15 * 60) time

-- | Connect to GDAX, Subscribe to the ETH-USD Ticker Channel & Run the
-- Specified Action with the Price Everytime we Receive a Message.
-- TODO: Have action take a rational or quantity
connect :: Currency -> (String -> IO ()) -> IO ()
connect currency action = run $ \connection -> do
    sendTextData connection . encode $ Subscribe currency
    forever $ do
        msg <- eitherDecode <$> receiveData connection
        case msg of
            Right (Ticker p) ->
                action p
            _ ->
                return ()
    where run =
            runSecureClient "ws-feed.gdax.com" 443 "/"


-- | The only request we care about is subscribing to feeds.
newtype GDAXRequest
    = Subscribe Currency

-- | Build the subscription request.
instance ToJSON GDAXRequest where
    toJSON (Subscribe (Currency symbol)) = object
        [ "type" .= ("subscribe" :: T.Text)
        , "product_ids" .= ([ T.pack $ symbol ++ "-USD" ] :: [T.Text])
        , "channels" .= ([ "ticker" ] :: [T.Text])
        ]


-- | The only response we care about is the ticker.
data GDAXResponse
    =  Ticker String
    | Unimplemented Value
    deriving (Show)


-- | Parse the ticker if that's what we get, otherwise return the
-- Unimplemented responsetype.
instance FromJSON GDAXResponse where
    parseJSON = withObject "GDAXResponse" $ \v -> do
        type_ <- v .: "type"
        if type_ == ("ticker" :: T.Text) then
            Ticker
                <$> v .: "price"
        else
            return $ Unimplemented $ Object v
