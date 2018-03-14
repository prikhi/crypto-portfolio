module App where

import Data.Maybe (listToMaybe)
import Brick

import qualified Graphics.Vty.Attributes as V

type AppState = ()

type AppEvent = ()

type AppWidget = ()

config :: App () AppEvent AppWidget
config =
    App
        { appDraw = const []
        , appChooseCursor = const listToMaybe
        , appHandleEvent = const . continue
        , appStartEvent = const $ return ()
        , appAttrMap = const $ attrMap V.defAttr []
        }


initialState :: AppState
initialState = ()
