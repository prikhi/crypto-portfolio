module Main where

import Brick.Main
import Control.Concurrent.Async (cancel)
import Control.Monad (void)

import App
import CoinTracking

import qualified Graphics.Vty as V


-- | Read the Trade CSV, Open the Price Streams, & Run the UI.
main :: IO ()
main = do
    transactions <- readTradeTableExport "trade_table.csv"
    (updateChannel, updateThreadIds) <- priceUpdateChannel transactions
    let appState = App.initialState transactions
    void $ customMain defaultVty (Just updateChannel) App.config appState
    mapM_ cancel updateThreadIds
    where defaultVty =
            V.mkVty V.defaultConfig
