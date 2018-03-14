module Main where

import Brick.Main

import App

main :: IO ()
main =
    defaultMain App.config App.initialState
