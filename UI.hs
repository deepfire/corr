{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE SignatureSections #-} 8.2
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module UI
    ( run
    -- Components:
    , WT(..), BSSwitch , FEditor, WTable
    )
where


-- Base imports
import           GHC.Generics                        (Generic)
import           Prelude.Unicode
import           Text.Printf                         (printf)


-- External imports
import qualified Brick.Main                       as B
import qualified Brick.Types                      as B
import qualified Brick.Util                       as B
import qualified Brick.Widgets.Core               as B
import qualified Brick.Widgets.Edit               as B
import qualified Brick.Widgets.List               as B
import qualified Brick.AttrMap                    as B
import           Brick.Widgets.Core                  ( str, fill
                                                     --, vBox, hBox
                                                     , vLimit, hLimit
                                                     , (<+>), (<=>)
                                                     )
import           Brick.Util                          (fg, bg)
import           Control.Lens                 hiding (At, argument, from, over)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Char                           (isSpace)
import           Data.Default                        (Default, def)
import           Data.Function                       (on)
import           Data.List                           (intercalate)
import           Data.List.Split                     (splitOn)
import           Data.Maybe                          (fromMaybe)
import           Data.Monoid                         ((<>))
import           Data.Hashable                       (Hashable)
import           Data.Time.Clock                     (getCurrentTime)
import           Data.Time.LocalTime                 (LocalTime(..), utcToLocalTime, hoursToTimeZone)
import qualified Data.Vector                      as V
import qualified Graphics.Vty                     as VT


-- Local imports
import           Columns
import           Molecule
import qualified Radio                            as B
import           Supplementary
import           Table
import           Work
import           Youtrack                     hiding (Field, State, Tag)
import           Youtrack.Extras


run ∷ [MolF] → IO [MolF]
run = B.defaultMain app

-- * Vty/Brick utilities
vty_evkey_remap ∷ [(VT.Key, VT.Key)] → VT.Event → VT.Event
vty_evkey_remap keymap ev@(VT.EvKey from mods) =
    case lookup from keymap of
      Nothing  → ev
      Just to' → VT.EvKey to' mods
vty_evkey_remap _ _ = error "Invariant failed:  request to remap a non-key event."

mkEnumRadio ∷ (Bounded a, Enum a) ⇒ Maybe String → (a → (String, a)) → Int → B.Radio a
mkEnumRadio mtitle labelfn choice' =
    B.radio (B.Name "") mtitle $ Just (choice', map labelfn $ enumFrom minBound)
enumRadioLabel ∷ Show a ⇒ Int → a → (String, a)
enumRadioLabel dropchars m = (drop dropchars $ show m, m)

class BrickSelector a where
    selector_index       ∷ a → Maybe Int
    selector_upper_bound ∷ a → Int
    selector_lower_bound ∷ a → Int
instance BrickSelector (B.List a) where
    selector_index       = B.listSelected
    selector_lower_bound = const 0
    selector_upper_bound = (\x → x - 1) ∘ V.length ∘ B.listElements
instance BrickSelector (Table a b) where
    selector_index       = tableSelRowIx
    selector_lower_bound = const 0
    selector_upper_bound = (\x → x - 1) ∘ V.length ∘ tableVisibleRows

brick_selector_updown_event_at_boundary ∷ BrickSelector a ⇒ VT.Event → a → Bool
brick_selector_updown_event_at_boundary key w =
    case key of
      VT.EvKey VT.KUp   [] | Just x ← selector_index w → x > selector_lower_bound w
      VT.EvKey VT.KDown [] | Just x ← selector_index w → x < selector_upper_bound w
      _ → False

with_updated_attrmap ∷ B.Widget → [(B.AttrName, VT.Attr)] → B.Widget
with_updated_attrmap w override = B.updateAttrMap (B.applyAttrMappings override) w

brick_hline, _brick_vline, _brick_hspace, _brick_vspace ∷ B.Widget
brick_hline  = vLimit 1 $ fill '─'
_brick_vline  = hLimit 1 $ fill '│'
_brick_hspace = vLimit 1 $ fill ' '
_brick_vspace = hLimit 1 $ fill ' '

brick_titled_separator ∷ B.AttrName → String → B.Widget
brick_titled_separator attr title_ = str "───* " <+> B.withAttr attr (str title_) <+> str " *─" <+> brick_hline


-- * (Work * Column)
pprint_work_oneline ∷ [WSColumn] → Work → String
pprint_work_oneline fws work =
    intercalate " "
    [ printf ("%" <> (show $ field_width_specifier w) <> "s") $ render_field w (work, "") -- Note [Issue-Work-Column interdependence] in Fields.hs
    | w ← fws ]

pprint_work_multiline ∷ [WSColumn] → Work → String
pprint_work_multiline fws work =
    intercalate "\n"
    [ printf "%-20s " (field_name w <> ":")
      <> (dropWhile isSpace $ render_field w (work, "")) -- Note [Issue-Work-Column interdependence] in Fields.hs
    | w ← fws ]


-- * Corr UI: work table screen

data               WT = WTSet | WTExpr | WTTable deriving (IsAF, Enum, Eq, Generic, Hashable, Ord, Show)
instance Molecule (Mol WT) (BranchSetKind, Worksets)

type               BSSwitch = Atm (BranchSetKind, B.Radio BranchSetKind)
type instance AFOf BSSwitch = WT
instance Atom      BSSwitch where
    type Data      BSSwitch = BranchSetKind
    type Widget    BSSwitch = B.Radio BranchSetKind
    derive_atom kind         = Atm (kind, mkEnumRadio (Just "branch set") (enumRadioLabel 0) (fromEnum kind))
    atom_event_filter _ fd _ = fd
    atom_handlers _          =
        [ (Key VT.KLeft,  handle, "previous branch set")
        , (Key VT.KRight, handle, "next branch set") ]
        where handle ∷ Handler WT BSSwitch
              handle = \mol ev (Atm (_bsk, w)) →
                       do w' ← B.handleEvent (vty_evkey_remap [ (VT.KRight,  VT.KChar '\t')
                                                              , (VT.KLeft,   VT.KBackTab) ] ev) w
                          -- XXX:  van Laarhoeven lens this! (?)
                          let addr = APt WTTable
                              bsk  = fromMaybe def $ B.radioSelection w'
                          with_molecule_atom mol addr
                              (\(Atm ((_oldbsk, wsets, vf), _) ∷ WTable) →
                               -- XXX: there is an fmap somewhere here!
                               pure [ UpdateAt addr ∘ AtmF $ (derive_atom (bsk, wsets, vf) ∷ WTable)
                                    , Update        ∘ AtmF $ Atm (bsk, w') ])
    render_atom focused (Atm (_, w)) =
        vLimit 1
        $ with_updated_attrmap (B.renderRadio w)
        $ if not focused then []
          else [(B.buttonSelectedAttr, vtaInvWhiteBG)]

filterHeadAttr, filterHeadAttrS ∷ B.AttrName
filterHeadAttr  = "flthead"
filterHeadAttrS = "fltheadS"

type               FEditor  = Atm ((), B.Editor)
type instance AFOf FEditor  = WT
instance Atom      FEditor  where
    type Data      FEditor  = ()
    type Widget    FEditor  = B.Editor
    derive_atom _            = Atm ((), B.editor (B.Name "") (str ∘ unlines) (Just 1) "")
    atom_event_filter _ _ _  = True
    atom_handlers _          =
        [ (CharKeys,   handle, "edit filter")
        , (Key VT.KBS, handle, "edit filter") ]
        where handle = \mol ev (Atm (_, w)) →
                       do w' ← B.handleEvent ev w
                          let filter_text = head $ B.getEditContents w'
                              filter_expr = filter (not ∘ null) $ splitOn " " filter_text
                              addr        = APt WTTable
                              vf          = (\(_, rowstr) → strHasTokens filter_expr rowstr)
                          with_molecule_atom mol addr
                              (\(Atm ((kind, wsets, _oldvf), _) ∷ WTable) →
                               pure [ UpdateAt addr ∘ AtmF $ (derive_atom (kind, wsets, vf) ∷ WTable)
                                    , Update        ∘ AtmF $ Atm ((), w') ])
    render_atom focused (Atm (_, w)) =
        (B.withAttr (if focused then filterHeadAttrS else filterHeadAttr)
         $ str "-->") <+> str " " <+> B.renderEditor w

worktable_fields ∷ [WSColumn]
worktable_fields = [ WSC SIssue, WSC SBranch, WSC SCommitter, WSC SCommitted, WSC SState, WSC SAssignee, WSC SWG ]

instance Default WSColumn where
    def = WSC SCommitted

type WorktableEntry = (Work, String)

instance Fieldy WorktableEntry WSColumn where
    render_field  WSC{..} = render_field' unwrap ∘ view _1
    compare_rows  WSC{..} = compare `on` work_field unwrap ∘ view _1
    default_order         = field_default_order

type               WTable   = Atm ((BranchSetKind, Worksets, WorktableEntry → Bool), Table WorktableEntry WSColumn)
type instance AFOf WTable   = WT
instance Atom      WTable   where
    type Data      WTable   = (BranchSetKind, Worksets, WorktableEntry → Bool)
    type Widget    WTable   = Table WorktableEntry WSColumn
    derive_atom dat@(bsk, wsets, visflt) =
        Atm ( dat
            , table
              (B.Name "")
              [ (drop 1 $ show vc, field_width_specifier cw, cw)
              | cw@(WSC vc) ← worktable_fields ]
              def
              visflt
              [ (w, pprint_work_oneline worktable_fields w)
              | w ← kind_set bsk wsets ] )
    atom_event_filter (Atm (_, w)) fd ev@(VT.EvKey k []) | k ≡ VT.KLeft ∨ k ≡ VT.KRight = fd
                                                         | k ≡ VT.KUp   ∨ k ≡ VT.KDown  = brick_selector_updown_event_at_boundary ev w
    atom_event_filter _            _  _                                                 = True
    atom_handlers  _         =
        [ (Key VT.KEnter,    enter,  "show branch/issue, with comments")
        , (Key VT.KLeft,     handle, "select previous sort column")
        , (Key VT.KRight,    handle, "select next sort column")
        , (Key VT.KUp,       handle, "one branch/issue upwards")
        , (Key VT.KDown,     handle, "one branch/issue downwards")
        , (Key VT.KPageUp,   handle, "one screen upwards")
        , (Key VT.KPageDown, handle, "one screen downwards") ]
        where handle = \_ ev (Atm (_dat, w)) →
                       (:[]) ∘ Update ∘ AtmF ∘ Atm ∘ (_dat,) <$> flip B.handleEvent w
                                 (vty_evkey_remap [ (VT.KRight,  VT.KChar '\t')
                                                  , (VT.KLeft,   VT.KBackTab) ] ev)
              enter  = \_ _  (Atm (_, w)) →
                       pure [ PushMolecule $ do
                                let work = flip fromMaybe (fst <$> tableSelectedRow w)
                                           $ error "table: no row selected?"
                                    i@Issue{..} = work_issue work
                                cs ← if fromIId issue_id ≢ 0 -- XXX: cheesy logic..
                                     then liftIO $ issue_comments i
                                     else pure []
                                pure $ molecule "Work" (VT.KUp, VT.KDown) True $
                                         (APt WVIssue, AtmF (derive_atom work ∷ IView))
                                         : [ (AVec WVComment nr, AtmF (derive_atom c ∷ IComment))
                                           | (nr, c) ← zip [0..] cs ] ]
    render_atom focused (Atm (_, w)) =
        B.Widget B.Fixed B.Greedy $ do
            ctx ← B.getContext
            let h = ctx ^. B.availHeightL -- limit, to avoid 'visible'-caused jitter
            B.render ∘ B.vLimit h
             $ with_updated_attrmap (renderTable w)
             $ if not focused then []
               else [(Table.tableColumnSelectedAttr, vtaInvWhiteBG)]


-- * Corr UI: work view screen

data               WV = WVIssue | WVComment deriving (IsAF, Enum, Eq, Generic, Hashable, Ord, Show)
instance Molecule (Mol WV) (Work, [Comment])

type               IView = Atm (Work, B.List String)
type instance AFOf IView = WV
instance Atom      IView where
    type Data      IView = Work
    type Widget    IView = B.List String
    derive_atom work     = Atm (work, B.list (B.Name "") (V.fromList ∘ lines $ pprint_work_multiline all_fields work) 1)
    atom_event_filter (Atm (_, w)) _ k =
        brick_selector_updown_event_at_boundary k w
    atom_handlers _      =
        [ (Key VT.KUp,   handle, "previous issue field -or- comment")
        , (Key VT.KDown, handle, "next issue field -or- comment") ]
        where handle _mol ev (Atm (dat, w)) = (:[]) ∘ Update ∘ AtmF ∘ Atm ∘ (dat,) <$> B.handleEvent ev w
    atom_focus   (Atm (_dat, w)) LT  = Atm ∘ (_dat,) $ w & B.listSelectedL .~ Just ((length $ B.listElements w) - 1)
    atom_focus   (Atm (_dat, w)) _   = Atm ∘ (_dat,) $ w & B.listSelectedL .~ Just 0
    atom_unfocus (Atm (_dat, w))     = Atm ∘ (_dat,) $ w & B.listSelectedL .~ Nothing
    render_atom focused (Atm (_, w)) =
        vLimit (1 + (V.length $ B.listElements w))
        $ B.renderList (w & if focused then id
                            else B.listSelectedL .~ Nothing)
        $ const str

instance B.HandleEvent ()       where
    handleEvent _ = pure
instance B.HandleEvent B.Widget where
    handleEvent _ = pure

type               IComment = Atm (Comment, ())
type instance AFOf IComment = WV
instance Atom      IComment where
    type Data      IComment = Comment
    type Widget    IComment = () -- Exception -- cannot handle events
    derive_atom c           = Atm (c, ())
    atom_handlers _         =
        [ (CharKey 'r',   handle, "reply to comment")
        , (Key VT.KEnter, handle, "reply to comment") ]
        where handle _g _e (Atm (c, ())) =
                  pure [ Spawn GT ∘ pure ∘ AtmF $ (derive_atom c ∷ IReply) ]
    render_atom focused (Atm (Comment{..}, ())) =
        let (headattr, bodyattr) = if focused then (commentHeadAttrS, commentBodyAttrS) else (commentHeadAttr, commentBodyAttr)
        in (brick_titled_separator headattr
            $ printf "comment by %s  %s"
                  (printFullNameEastern ∘ fromMFullName ∘ member_fullname $ comm_author)
                  (show $ fromMaybe comm_created comm_updated))
           <=> (B.withAttr bodyattr $ str (comm_text <> "\n "))

commentHeadAttrS, commentBodyAttrS, commentHeadAttr, commentBodyAttr ∷ B.AttrName
commentHeadAttrS = "comheadS"
commentBodyAttrS = "combodyS"
commentHeadAttr  = "comhead"
commentBodyAttr  = "combody"

-- XXX: shouldn't we separate unaccessible projects at type level?
mkCommentPost ∷ Issue → LocalTime → String → Comment
mkCommentPost comm_issue@Issue{..} comm_created comm_text =
    let comm_author  = the_project_accessor_for issue_project $ "comment on issue %s" <> issue_idstr comm_issue
        comm_updated = Nothing
    in Comment{..}

type                IReply = Atm (Comment, B.Editor)
type instance  AFOf IReply = WV
instance Atom       IReply where
    type Data       IReply = Comment
    type Widget     IReply = B.Editor
    derive_atom (c@Comment{..}) = Atm (c, B.editor (B.Name "") (str ∘ unlines) (Just 10) -- XXX:  arbitrary editor height limit!
                                          $ "@" <> (fromMLogin ∘ member_login $ comm_author) <> ", ")
    atom_handlers self     =
        [ (Key    VT.KEsc,              handle, "cancel current comment posting")
        , (ModKey VT.KEnter [VT.MMeta], handle, "post reply")
        , (CharKeys,                    handle, "edit reply")
        , (NonCharKeys,                 handle, "edit reply") ]
        where handle _mol (VT.EvKey VT.KEnter [VT.MMeta]) (Atm (Comment{..}, w)) =
                  do time ← liftIO $ fmap (utcToLocalTime (hoursToTimeZone 3)) getCurrentTime
                     let comment = mkCommentPost comm_issue time (concat $ B.getEditContents w)
                     liftIO $ issue_post_comment comm_issue comment
                            -- XXX:  queue an update instead of Change -- more honest that way (albeit less fluid..)
                     pure [ Change (AtmF $ (derive_atom comment ∷ IComment) ∷ AtmF (AFOf IComment)) ]
              handle _mol (VT.EvKey VT.KEsc []) _ =
                  pure [ Disappear (AtmF self) ]
              handle _mol ev a = handle_editor ev a
              handle_editor ev (Atm (c,w)) = (:[]) ∘ Update ∘ AtmF ∘ Atm ∘ (c,) <$> B.handleEvent ev w
    render_atom focused (Atm (Comment{..}, w)) =
        (brick_titled_separator (if focused then commentHeadAttrS else commentHeadAttr)
         $ printf "replying to %s  %s"
               (printFullNameEastern ∘ fromMFullName ∘ member_fullname $ comm_author)
               (show $ fromMaybe comm_created comm_updated))
        <=> B.renderEditor w


vtaCyanFG, vtaInvCyanBG, vtaInvWhiteBG, vtaDimFG, vtaPlain ∷ VT.Attr
vtaCyanFG     = fg VT.cyan
vtaInvCyanBG  = VT.black `B.on` VT.cyan
vtaInvWhiteBG = bg VT.white
vtaDimFG      = VT.defAttr `VT.withStyle` VT.bold
vtaPlain      = VT.defAttr

app ∷ B.App [MolF] VT.Event
app = B.App
      { B.appLiftVtyEvent = id
      , B.appStartEvent   = pure
      , B.appHandleEvent  = molecules_handle_event
      , B.appDraw         = \case
                            []           → [] -- XXX:  this "cannot" happen and makes Brick raise an error
                            (MolF gr : _) → molecule_draw gr
      --
      , B.appChooseCursor = B.showFirstCursor -- XXX:  this is where the cursor is chosen
      , B.appAttrMap      = const $ B.attrMap VT.defAttr
                            [ ("", (⊥))
                            -- Worktable
                            , (B.buttonSelectedAttr,            vtaInvCyanBG)
                            , (filterHeadAttr,                  vtaPlain)
                            , (filterHeadAttrS,                 vtaInvWhiteBG)
                            , (Table.tableColumnSelectedAttr,   vtaInvCyanBG)
                            , (Table.tableRowSelectedAttr,      vtaCyanFG)
                            -- Workview
                            , (B.listSelectedAttr,              vtaCyanFG)
                            , (commentHeadAttr,                 vtaPlain)
                            , (commentBodyAttr,                 vtaDimFG)
                            , (commentHeadAttrS,                vtaCyanFG)
                            , (commentBodyAttrS,                vtaPlain)
                            ]
      }
