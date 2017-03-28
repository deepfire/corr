{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module provides a simple radio widget. You get to pick the
-- radio title, if any, as well as its buttons.
module Radio
  ( Radio
  , radioTitle
  , radioName
  , radioButtons
  , radioSelectedIndex
  -- * Construction, rendering, events
  , radio
  , renderRadio
  , handleEventRadio
  -- * Getting a radio's current value
  , radioSelection
  -- * Attributes
  , radioAttr
  , buttonAttr
  , buttonSelectedAttr
  -- * Lenses
  , radioNameL
  , radioButtonsL
  , radioSelectedIndexL
  , radioTitleL
  )
where

import Control.Lens
import Data.Monoid
import Data.List (intersperse)
import Graphics.Vty.Input (Event(..), Key(..))

import Brick.Util (clamp)
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.AttrMap

-- | Radios present a window with a title (optional), and
-- buttons (optional). They provide a 'HandleEvent' instance that knows
-- about Tab and Shift-Tab for changing which button is active. Radio
-- buttons are labeled with strings and map to values of type 'a', which
-- you choose.
--
-- Radios handle the following events by default:
--
-- * Tab: selecte the next button
-- * Shift-tab: select the previous button
data Radio n a =
    Radio { radioName          :: n             -- ^ The radio name
          , radioTitle         :: Maybe String  -- ^ The radio title
          , radioButtons       :: [(String, a)] -- ^ The radio button labels and values
          , radioSelectedIndex :: Maybe Int     -- ^ The currently selected radio button index (if any)
          }

suffixLenses ''Radio

handleEventRadio ev d =
    case ev of
        VtyEvent (EvKey (KChar '\t') []) -> pure $ nextButtonBy 1 d
        VtyEvent (EvKey KBackTab [])     -> pure $ nextButtonBy (-1) d
        _ -> pure d

-- | Create a radio.
radio ::  n                          -- ^ The radio name, provided so that you can use this as a
                                     --   basis for viewport names in the radio if desired
       -> Maybe String               -- ^ The radio title
       -> Maybe (Int, [(String, a)]) -- ^ The currently-selected button index (starting at zero) and
                                     --   the button labels and values to use
       -> Radio n a
radio name title buttonData =
    let (buttons, idx) = case buttonData of
          Nothing -> ([], Nothing)
          Just (_, []) -> ([], Nothing)
          Just (i, bs) -> (bs, Just $ clamp 0 (length bs - 1) i)
    in Radio name title buttons idx

-- | The default attribute of the radio
radioAttr :: AttrName
radioAttr = "radio"

-- | The default attribute for all radio buttons
buttonAttr :: AttrName
buttonAttr = "button"

-- | The attribute for the selected radio button (extends 'radioAttr')
buttonSelectedAttr :: AttrName
buttonSelectedAttr = buttonAttr <> "selected"

-- | Render a radio
renderRadio :: Radio n a -> Widget n
renderRadio d =
    let buttonPadding = str ""
        mkButton (i, (s, _)) = let att = if Just i == d^.radioSelectedIndexL
                                         then buttonSelectedAttr
                                         else buttonAttr
                               in withAttr att $ str $ " " <> s <> " "
        buttons = hBox $ (case radioTitle d of
                            Nothing -> []
                            Just t  -> [str (t <> ": ")])
                  <> (intersperse buttonPadding $ mkButton
                     <$> (zip [0..] (d^.radioButtonsL)))
    in center $
       withDefAttr radioAttr $
       buttons

nextButtonBy :: Int -> Radio n a -> Radio n a
nextButtonBy amt d =
    let numButtons = length $ d^.radioButtonsL
    in if numButtons == 0 then d
       else case d^.radioSelectedIndexL of
           Nothing -> d & radioSelectedIndexL .~ (Just 0)
           Just i -> d & radioSelectedIndexL .~ (Just $ (i + amt) `mod` numButtons)

-- | Obtain the value associated with the radio's currently-selected
-- button, if any. This function is probably what you want when someone
-- presses 'Enter' in a radio.
radioSelection :: Radio n a -> Maybe a
radioSelection d =
    case d^.radioSelectedIndexL of
        Nothing -> Nothing
        Just i -> Just $ ((d^.radioButtonsL) !! i)^._2
