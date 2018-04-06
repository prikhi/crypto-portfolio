{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Table
    ( table
    , TableConfig(..)
    , Column(..)
    , Alignment(..)
    , VerticalAlignment(..)
    , HorizontalAlignment(..)
    , updateTable
    , tableHeaderAttr
    , tableFooterAttr
    , renderTable
    ) where

import Brick
import Control.Lens ((^.))
import Data.List (elemIndex, intersperse, intercalate)
import Data.Maybe (isJust, fromJust)

import Prelude hiding (Either(..))
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as B
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

-- TODO: Support Mouse Wheel Scrolling in Body Viewport

-- | Table rendering options for the Brick widget.
data TableConfig a n
    = TableConfig
        { columns :: [Column a]
        , showRowDividers :: Bool
        , footerRows :: [[Widget n]]
        , name :: n
        }

instance Named (TableConfig a n) n where
    getName = name

-- | The formatting information for a Column in the Table.
data Column a
    = Column
        { headerName :: String
        , headerAlign :: Alignment
        , dataAlign :: Alignment
        , columnWeight :: Weight
        , dataSelector :: a -> String
        }

-- | The Horizontal & Vertical Alignment of a cell.
data Alignment
    = Alignment
        { verticalAlign :: VerticalAlignment
        , horizontalAlign :: HorizontalAlignment
        }

-- | Vertical Alignment controls the height padding within a cell.
data VerticalAlignment
    = VTop
    | VMiddle
    | VBottom
    deriving (Show, Eq)

-- | Horizontal alignment controls the width padding within a cell.
data HorizontalAlignment
    = HLeft
    | HCenter
    | HRight
    deriving (Show, Eq)


-- TODO: Is this just ignored by the even amount of expansion per column?
type Weight
    = Integer

type Width
    = Integer

type Height
    = Integer


-- UPDATE

-- | Scroll the Table body using vim keys.
updateTable :: Ord n => n -> V.Event -> EventM n ()
updateTable n = \case
    V.EvKey (V.KChar 'j') [] ->
        vScrollBy vp 1
    V.EvKey (V.KChar 'k') [] ->
        vScrollBy vp (-1)
    V.EvKey (V.KChar 'g') [] ->
        vScrollToBeginning vp
    V.EvKey (V.KChar 'G') [] ->
        vScrollToEnd vp
    V.EvKey (V.KChar 'd') [V.MCtrl] ->
        scrollByPage 0.5 n
    V.EvKey (V.KChar 'u') [V.MCtrl] ->
        scrollByPage (-0.5) n
    V.EvKey (V.KChar 'f') [V.MCtrl] ->
        vScrollPage vp Down
    V.EvKey (V.KChar 'b') [V.MCtrl] ->
        vScrollPage vp Up
    _ ->
        return ()
    where
        vp =
            viewportScroll n

-- | Scroll the viewport by the given amount of pages.
scrollByPage :: Ord n => Rational -> n -> EventM n ()
scrollByPage pages vpName = do
    v <- lookupViewport vpName
    case v of
        Nothing ->
            return ()
        Just vp ->
            vScrollBy (viewportScroll vpName)
                . round . (* pages) . toRational . snd $ _vpSize vp


-- VIEW

-- | The Brick Attribute for the Table Header Row.
tableHeaderAttr :: AttrName
tableHeaderAttr =
    attrName "table-header"

-- | The Brick Attribute for each Table Footer Row.
tableFooterAttr :: AttrName
tableFooterAttr =
    attrName "table-footer"

-- | Render a Table using a Config & a List of Items.
table :: (Show n, Ord n) => TableConfig a n -> [a] -> Widget n
table config items =
    Widget Fixed Fixed $ do
        context <- getContext
        let width = context ^. availWidthL - length (columns config) + 1
            height = context ^. availHeightL
        render $ renderTable config (fromIntegral width) (fromIntegral height) items


-- | Render a fixed-size table.
renderTable :: (Show n, Ord n) => TableConfig a n -> Width -> Height -> [a] -> Widget n
renderTable config w h is =
    vBox
        $ withHeader
        $ withFooter
        $ vLimit (min (length rows * 2 - 1) $ fromIntegral h)
        $ viewport (name config) Vertical
        $ vBox
        $ intersperse (cropToContext B.hBorder) rows
    where
        rows =
            map (formatRow cs widths) is
        withHeader rs =
            if showRowDividers config then
                withAttr tableHeaderAttr (formatHeader cs widths)
                    : boldBorder
                    : rs
            else
                withAttr tableHeaderAttr (formatHeader cs widths)
                    : rs
        withFooter rs =
            if showRowDividers config && not (null $ footerRows config) then
                rs : boldBorder : footer
            else
                rs : footer
        footer =
            map footerRow $ footerRows config
        footerRow r =
            hBox
                . intersperse (str " ")
                $ zipWith footerColumn (map fromIntegral widths) r
        footerColumn width col =
            withAttr tableFooterAttr
                . hLimit width
                $ padLeft Max col
        boldBorder =
            cropToContext $ withBorderStyle B.unicodeBold B.hBorder
        widths =
            if availableSpacePerColumn > 0 then
                map (+ availableSpacePerColumn) shrunkWidths
            else
                shrunkWidths
        availableSpacePerColumn =
            (w - sum shrunkWidths) `div` toInteger (length cs)
        shrunkWidths =
            shrinkToMaxRowLengths cs is $ makeWeights cs is w
        cs =
            columns config

-- | Render the Table's Header as a Brick widget.
formatHeader :: [Column a] -> [Width] -> Widget n
formatHeader cs ws =
    hBox
        . intersperse (str " ")
        . zipWith3 (align 1) ws (map headerAlign cs)
        $ map (\c -> [headerName c]) cs

-- | Render an Item's row as a Brick widget.
formatRow :: [Column a] -> [Width] -> a -> Widget n
formatRow cs ws i =
    hBox
        . intersperse (str " ")
        $ zipWith3 (align rowHeight) ws (map dataAlign cs) wrapped
    where
        rowHeight =
            toInteger . maximum $ map length wrapped
        wrapped =
            zipWith wrap ws values
        values =
            map (`dataSelector` i) cs



-- TODO: The rest of this is pretty much ripped from my hkredmine
-- FormatTable code. I should refactor this into it's own package & use it
-- in both programs.


-- Width Generation

-- | Make a list of column Widths by starting from the minimums and
-- increasing widths in search of the ideal width ratio defined by the
-- column weights.
makeWeights :: [Column a] -> [a] -> Width -> [Width]
makeWeights cs is w =
    build minimumWidths
    where
        minimumWidths =
            map (minimumWidth is) cs
        targetWeight =
            w - toInteger (3 * (length cs - 1))
        ratios ws =
            map ((/ toRational (sum ws)) . toRational) ws
        weights =
            map columnWeight cs
        build current =
            if sum current < targetWeight && isJust bumpIndex then
                build $ take (fromJust bumpIndex) current
                ++ current !! fromJust bumpIndex + 1
                : drop (fromJust bumpIndex + 1) current
            else
                current
            where
                bumpIndex  = elemIndex (maximum ratioDiffs) ratioDiffs
                ratioDiffs = zipWith (-) (ratios weights) (ratios current)
-- | Reduce a list of column widths to the length of the column's longest
-- row.
shrinkToMaxRowLengths :: [Column a] -> [a] -> [Width] -> [Width]
shrinkToMaxRowLengths cs is     = zipWith shrinkColumn cs
        where shrinkColumn c w  = min w . maximum . map (toInteger . length) .
                                  concatMap (wrap w) $ getColumnContents c is

-- | Determine the minimum width of a column by finding it's longest word.
minimumWidth :: [a] -> Column a -> Width
minimumWidth is c   = maximum . concatMap (map (fromIntegral . length) . words)
                    $ getColumnContents c is

-- | Get all the data in a column, including the header.
getColumnContents :: Column a -> [a] -> [String]
getColumnContents c is  = headerName c : map (dataSelector c) is


-- Line Wrapping

-- | Wrap a string into multiple lines by words given a maximum width.
wrap :: Width -> String -> [String]
wrap width              = wrapLine width [] "" . words

-- | Recursively wrap a line by words, using the width, current result, current
-- word and a list of words to process.
wrapLine :: Width -> [String] -> String -> [String] -> [String]
wrapLine _ result c []      = result ++ [c]
wrapLine w result c (i:is)
        | newLength > w &&
          null c            = wrapLine w result i is
        | newLength > w     = wrapLine w (result ++ [c]) i is
        | newLength <= w &&
          c == ""           = wrapLine w result i is
        | otherwise         = wrapLine w result (c ++ " " ++ i) is
        where  newLength    = toInteger . length $ c ++ " " ++ i


-- Alignment

-- | Align a wrapped string horizantally and vertically.
align :: Height -> Width -> Alignment -> [String] -> Widget n
align h w a ss =
    hLimit (fromIntegral w) $ vLimit (fromIntegral h)
        . alignVertically (verticalAlign a)
        . str
        . handleBlank
        . intercalate "\n"
        $ map (alignHorizontally w (horizontalAlign a)) ss
    where
        handleBlank s =
            if s == "" then
                " "
            else
                s

-- | Horizaontally align a String, given a HorizontalAlignment and a Width.
-- TODO: Make this work using C.hCenter? Couldn't get it to center justify
-- multiple-lines.
alignHorizontally :: Integer -> HorizontalAlignment -> String -> String
alignHorizontally w h s
    | h == HLeft =
        s ++ replicate x ' '
    | h == HRight =
        replicate x ' ' ++ s
    | otherwise =
        replicate l ' ' ++ s ++ replicate r ' '
    where
        x = fromIntegral w - length s
        l = x `div` 2
        r = x - l

-- | Vertically align a Widget, given a VerticalAlignment and a Height
alignVertically :: VerticalAlignment -> Widget n -> Widget n
alignVertically v = case v of
    VTop ->
        padBottom Max
    VMiddle ->
        C.vCenter
    VBottom ->
        padTop Max
