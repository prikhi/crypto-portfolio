module Main where

import Brick.Main
import Control.Monad (void)

import App


main :: IO ()
main =
    void $ defaultMain App.config App.initialState
