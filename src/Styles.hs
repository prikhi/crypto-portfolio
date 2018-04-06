module Styles where

import Brick (AttrMap, attrMap)

import qualified Graphics.Vty as V

import Table (tableHeaderAttr, tableFooterAttr)

-- | The Style Map for the Application.
--
-- Currently we only use the terminal's default values.
attributeMap :: AttrMap
attributeMap =
    attrMap V.defAttr
        [ ( tableHeaderAttr, V.withStyle V.defAttr V.bold )
        , ( tableFooterAttr, V.withStyle V.defAttr V.bold )
        ]
