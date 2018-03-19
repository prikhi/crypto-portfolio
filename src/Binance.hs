{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Binance where

import Control.Monad (forever)
import Data.Aeson ((.:), FromJSON(..), eitherDecode, withObject, Value(Object))
import Data.Char (toLower)
import Network.WebSockets (receiveData)
import Wuss (runSecureClient)

import qualified Data.Text as T


-- | Connect to Binance, Subscribe to an 24hr Ticker Stream & Run the
-- Specified Action with the Price Everytime we Receive a Message.
--
-- TODO: When models refactored into separate model, pass currency list
-- & use combined streams.
--
-- TODO: Or maybe just always pull all of binance's prices? See how much
-- network that eats up.
connect :: String -> (String -> IO ()) -> IO ()
connect symbol action = run $ \connection ->
    forever $ do
        msg <- eitherDecode <$> receiveData connection
        case msg of
            Right (Ticker p) ->
                action p
            _ ->
                return ()
    where
        run =
            runSecureClient "stream.binance.com" 9443 ("/ws/" ++ streamName)
        streamName =
            map toLower symbol ++ "eth@ticker"



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
