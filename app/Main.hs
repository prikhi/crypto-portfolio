module Main where

import Brick.Main
import Control.Concurrent (killThread)
import Control.Monad (void)

import App
import CoinTracking

import qualified Graphics.Vty as V


main :: IO ()
main = do
    transactions <- readTradeTableExport "trade_table.csv"
    (updateChannel, updateThreadIds) <- priceUpdateChannel transactions
    let appState = App.initialState transactions
    void $ customMain defaultVty (Just updateChannel) App.config appState
    mapM_ killThread updateThreadIds
    where defaultVty =
            V.mkVty V.defaultConfig
