module Main where

import Brick.Main
import Control.Monad (void)

import App


main :: IO ()
main =
    App.initialState >>= void . defaultMain App.config
