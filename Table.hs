{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | This module provides a simple table widget. You get to pick the
-- table title, if any, as well as its columns.
module Table
  ( Fieldy(..)
  -- , Fieldy'(..)
  -- , ColumnW(..)
  -- , field_name, field_width, field_default_order
  -- * Table
  , Table
  , tableName
  , tableOrder
  , tableCols
  , tableSelRowIx
  , tableSelColIx
  , tableVisibleRows
  -- * Construction, filtering, rendering, event handling
  , table
  , tableSetSrcRows
  , tableSetVisibilityFilter
  , handleEventTable
  , renderTable
  -- * Getting a table's current value
  , tableSelectedRow
  , tableSelectedColumn
  -- * Attributes
  , tableAttr
  , tableRowAttr
  , tableRowSelectedAttr
  , tableColumnAttr
  , tableColumnSelectedAttr
  )
where

import           Control.Arrow
import           Control.Lens
import           Data.Default
import           Data.List                 (findIndex, intercalate, intersperse, sortBy)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Vector            as V
import           Graphics.Vty.Input        (Event(..), Key(..))

import           Brick.AttrMap
import           Brick.Main                (lookupViewport)
import           Brick.Types
import           Brick.Util                (clamp)
import           Brick.Widgets.Core

import           Prelude.Unicode

-- import           Debug.Trace            (trace)
import           Text.Printf               (printf)

-- | Tables present a window with a title (optional), and
-- buttons (optional). They provide a 'HandleEvent' instance that knows
-- about Tab and Shift-Tab for changing which button is active. Table
-- buttons are labeled with strings and map to values of type 'a', which
-- you choose.
--
-- Tables handle the following events by default:
--
-- * Tab: selecte the next button
-- * Shift-tab: select the previous button


-- * Tools, supposedly belonging elsewhere
centerString ∷ Int → String → String
centerString width string =
    let (half, rest) = divMod (width - length string) 2
        pad        n = take n $ repeat ' '
    in pad half <> string <> pad (half + rest)


-- * Field machinery

class (Default col, Eq col) ⇒ Fieldy row col | col → row where
    render_field  ∷ col → row → String
    compare_rows  ∷ col → row → row → Ordering
    default_order ∷ col → Ordering


-- * Table to bind them all

data Table n row col
    = Fieldy row col ⇒
      Table
          { tableName             ∷ n                           -- ^ The table name
          , tableOrder            ∷ Ordering                    -- ^ Sort ordering.  EQ ≡ GT
          , tableSrcRows          ∷ !(V.Vector row)  -- listElements
          , tableCols             ∷ [(String, Int, col)]        -- ^ The table column labels, widths (negative/positive
                                                                --   interpreted as left/right adjustment) and values
          , tableSelRowIx         ∷ !(Maybe Int)                -- ^ The currently selected table row index (if any)
          , tableSelColIx         ∷ !Int                        -- ^ The currently selected table column index (if any)
          , tableVisibilityFilter ∷ row → Bool                  -- ^ Given a row, determine whether to show it
          , tableVisibleRows      ∷ !(V.Vector row) }           -- ^ Rows that satisfy the row visibility predicate

makePrisms ''Table

-- | Create a table.
table ∷ Fieldy row col
       ⇒ n                    -- ^ The table name -- a basis for viewport names in the table if desired
       → [(String, Int, col)] -- ^ The column labels and values to use
       → col
       → (row → Bool)
       → [row]
       → Table n row col
table name cols defcol vis_filter xs =
    let xs'    = V.fromList ∘ snd ∘ order_rows True defcol Nothing $ xs
        scix   = flip fromMaybe (findIndex ((≡ defcol) ∘ (^._3)) cols)
                 (error "The specified default column isn't present in the column list.")
        srix   = if length xs' ≡ 0 then Nothing else Just 0
    in Table name (default_order defcol) xs' cols srix scix vis_filter $ V.filter vis_filter xs'

negate_comparator ∷ (a → a → Ordering) → a → a → Ordering
negate_comparator f x y = case f x y of
                            EQ → EQ
                            GT → LT
                            LT → GT

order_rows ∷ Fieldy row col ⇒ Bool → col → Maybe Ordering → [row] → (Ordering, [row])
order_rows field_switch col orderchoice rows =
    let order      = if field_switch ∨ isNothing orderchoice
                     then default_order col
                     else fromJust orderchoice
        comparator = case order of
                       GT → compare_rows col
                       EQ → (\_ _ → EQ)
                       LT → negate_comparator $ compare_rows col
    in (order
       ,sortBy comparator rows)

handleEventTable (VtyEvent (EvKey key [])) prew =
    let field_switched   = key ≡ KChar '\t'
        (reorder, w)     = case key of
                             KChar '\t' → (True,  prew & nextColumnBy 1)
                             KBackTab   → (True,  prew & nextColumnBy (-1))
                             KChar '<'  → (True,  prew { tableOrder = LT })
                             KChar '>'  → (True,  prew { tableOrder = GT })
                             _          → (False, prew)
        (order, datavec) = (id *** V.fromList) ∘ order_rows field_switched (tableSelectedColumn w) (Just $ tableOrder w)
                           $ V.toList $ tableSrcRows w
        w'               = tableSetVisibilityFilter (tableVisibilityFilter w) $
                           w { tableOrder   = order
                             , tableSrcRows = datavec }
    in if | reorder   → pure w'
          | otherwise →
              case key of
                KUp          → pure $ w' & tableMoveUp
                KDown        → pure $ w' & tableMoveDown
                KHome        → pure $ w' & tableMoveTo 0
                KEnd         → pure $ w' & tableMoveTo (length $ tableVisibleRows w')
                KPageDown    → do mvport ← lookupViewport $ tableName w'
                                  case mvport of
                                    Nothing → pure w'
                                    Just v  → pure $ tableMoveBy (v^.vpSize._2) w'
                KPageUp      → do mvport ← lookupViewport $ tableName w'
                                  case mvport of
                                    Nothing → pure w'
                                    Just v  → pure $ tableMoveBy (negate $ v^.vpSize._2) w'
                _            → pure w'
handleEventTable _ w = pure w

-- | Render a table
renderTable ∷ (Ord n, Show n) ⇒ Fieldy row col ⇒ Table n row col → Widget n
renderTable Table{..} =
    let render_column_header_button (idx, (coltitle, width_spec, _)) =
            let isselected = idx == tableSelColIx
                attr       = if isselected
                             then withAttr tableColumnSelectedAttr
                             else withAttr tableColumnAttr
            in attr ∘ str $
               centerString (abs width_spec)
               (coltitle <> if not isselected
                            then ""
                            else case tableOrder of
                                   LT → " ↑"
                                   GT → " ↓"
                                   EQ → " =")
        render_column_header =
            hBox ∘ intersperse (str " ") $ render_column_header_button <$> zip [0..] tableCols
        render_row isselected el =
            let attr = if isselected
                       then withAttr (tableRowSelectedAttr)
                       else id
            -- XXX: no enforcement of field width
            in attr ∘ str $ intercalate " " [ printf ("%" <> show width_spec <> "s") $ render_field wcol el
                                            | (_, width_spec, wcol) ← tableCols ]
        render_data_rows =
            Widget Greedy Greedy $ do
                c ← getContext

                let nrows        = length tableVisibleRows
                    nshowable    = availHeight c
                    xselected    = fromMaybe 0 tableSelRowIx
                    xstart       = max 0 $ xselected - nshowable + 1
                    ntoshow      = min nshowable (nrows - xstart)
                    rowslice     = V.slice xstart ntoshow tableVisibleRows
                    xoff         = xstart --  * (l^.listItemHeightL)

                    drawnElements = flip imap rowslice $ \i r →
                        let isSelected  = Just (i + xstart) ≡ tableSelRowIx
                            elemWidget  = render_row isSelected r
                            makeVisible = if isSelected
                                          then visible ∘ withDefAttr tableRowSelectedAttr
                                          else id
                        in makeVisible elemWidget

                render $ viewport tableName Vertical $
                       translateBy (Location (0, xoff)) $
                       vBox $ V.toList drawnElements
    in withDefAttr tableAttr $
       vBox [ render_column_header
            , withDefAttr tableRowAttr $
              render_data_rows ]

tableAttr, tableRowAttr, tableColumnAttr, tableRowSelectedAttr, tableColumnSelectedAttr ∷ AttrName
tableAttr               = "table"                       -- ^ The default attribute of the table
tableRowAttr            = "row"                         -- ^ The default attribute for all table rows
tableRowSelectedAttr    = tableRowAttr <> "selected"    -- ^ The attribute for the selected table row (extends 'tableAttr')
tableColumnAttr         = "column"                      -- ^ The default attribute for all table columns
tableColumnSelectedAttr = tableColumnAttr <> "selected" -- ^ The attribute for the selected table column (extends 'tableAttr')

tableSetSrcRows ∷ V.Vector row → Table n row col → Table n row col
tableSetSrcRows rows w@Table{..} = tableSetVisibilityFilter tableVisibilityFilter $
                                   w { tableSrcRows  = rows
                                     , tableSelRowIx = if length rows > 0
                                                       then Just 0 else Nothing }

tableSetVisibilityFilter ∷ (row → Bool) → Table n row col → Table n row col
tableSetVisibilityFilter row_filter w@Table{..} =
    let visible_rows = V.filter row_filter tableSrcRows
    in w { tableVisibilityFilter = row_filter
         , tableVisibleRows      = visible_rows
         , tableSelRowIx         = fmap (clamp 0 $ length visible_rows) $ tableSelRowIx }

nextColumnBy ∷ Int → Table n row col → Table n row col
nextColumnBy amt w@Table{..} =
    let numColumns = length tableCols
    in if numColumns ≡ 0 then w
       else w { tableSelColIx = (tableSelColIx + amt) `mod` numColumns }

tableMoveUp, tableMoveDown ∷ Table n r c → Table n r c
tableMoveUp   = tableMoveBy (-1)
tableMoveDown = tableMoveBy 1

tableMoveBy ∷ Int → Table n r c → Table n r c
tableMoveBy n w =
    let nrows     = length $ tableVisibleRows w
        xselected = fmap (clamp 0 (nrows - 1) ∘ (+ n)) $ tableSelRowIx w
    in w { tableSelRowIx = xselected }


-- | Set the selected index for a list to the specified index, subject
-- to validation.
tableMoveTo ∷ Int → Table n r c → Table n r c
tableMoveTo x w =
    let nrows     = length $ tableVisibleRows w
        xto       = if x < 0 then nrows - x else x
        xselected = clamp 0 (nrows - 1) xto
    in w { tableSelRowIx = if nrows > 0
                           then Just xselected else Nothing }

-- | Obtain the value associated with the table's currently-selected
-- button, if any. This function is probably what you want when someone
-- presses 'Enter' in a table.
tableSelectedColumn ∷ Table n row col → col
tableSelectedColumn Table{..} =
    (tableCols !! tableSelColIx) ^. _3

tableSelectedRow ∷ Table n row col → Maybe row
tableSelectedRow Table{..} = fmap (tableVisibleRows V.!) tableSelRowIx
