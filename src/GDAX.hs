{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GDAX where

import Control.Monad (forever)
import Data.Aeson
    ( ToJSON(..), FromJSON(..), (.=), (.:), encode, eitherDecode, object
    , withObject, Value(Object)
    )
import Network.WebSockets (receiveData, sendTextData)
import Wuss (runSecureClient)

import qualified Data.Text as T


-- | Connect to GDAX, Subscribe to the ETH-USD Ticker Channel & Run the
-- Specified Action with the Price Everytime we Receive a Message.
-- TODO: Have action take a rational or quantity - requires more modules
connect :: (String -> IO ()) -> IO ()
connect action = run $ \connection -> do
    sendTextData connection $ encode Subscribe
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
