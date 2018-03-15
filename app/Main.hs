module Main where

import Brick.Main
import Control.Concurrent (killThread)
import Control.Monad (void)

import App

import qualified Graphics.Vty as V


main :: IO ()
main = do
    (tickChannel, tickThreadId) <- makeTickChannel
    App.initialState >>= void . customMain (V.mkVty V.defaultConfig) (Just tickChannel) App.config
    killThread tickThreadId
