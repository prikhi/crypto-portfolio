{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GDAX where

import Control.Exception (bracket)
import Control.Monad (forever)
import Data.Aeson
    ( ToJSON(..), FromJSON(..), (.=), (.:), encode, decode, eitherDecode
    , object,  withObject, Value(Object)
    )
import Network.WebSockets (receiveData, sendClose, sendTextData)
import Wuss (runSecureClient)

import qualified Data.Text as T


-- | Connect to GDAX, Subscribe to the ETH-USD Ticker Channel & Run the
-- Specified Action with the Price Everytime we Receive a Message.
connectAndSubscribe :: (String -> IO ()) -> IO ()
connectAndSubscribe action = connect $ \connection ->
    bracket (return connection) (`sendClose` ("" :: T.Text)) $ \_ -> do
        sendTextData connection $ encode Subscribe
        (_ :: Maybe GDAXResponse) <- decode <$> receiveData connection
        forever $ do
            msg <- eitherDecode <$> receiveData connection
            case msg of
                Right (Ticker p) ->
                    action p
                _ ->
                    return ()
    where connect =
            runSecureClient "ws-feed.gdax.com" 443 "/"


-- | The only request we care about is subscribing to feeds.
data GDAXRequest
    = Subscribe

-- | Build the subscription request.
instance ToJSON GDAXRequest where
    toJSON _ = object
        [ "type" .= ("subscribe" :: T.Text)
        , "product_ids" .= ([ "ETH-USD" ] :: [T.Text])
        , "channels" .= ([ "ticker" ] :: [T.Text])
        ]


-- | The only response we care about is the ticker.
data GDAXResponse
    =  Ticker
        { tickPrice :: String
        }
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
