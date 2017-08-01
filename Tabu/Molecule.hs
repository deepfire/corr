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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tabu.Molecule
    ( Address(..), AFOf, IsAF
    , Name(..)
    , EventM, HandleEvent(..)
    , KeySpec(..)
    , Molecule, Mol(..), MolF(..)
    , Atom(..), Atm(..),  AtmF(..)
    , Reaction(..)
    , MolEventRoute, MolAtomMap
    , molecule
    , molecule_draw
    , molecule_set_popup
    , Handler
    , molecules_handle_event
    , with_molecule_atom
    )
where

import           GHC.Generics                        (Generic)
import           Prelude.Unicode

import           Control.Lens                 hiding (At, argument, from, over)
import           Data.Data                           (Typeable, cast)
import           Data.Hashable                       (Hashable)
import           Data.Function
import qualified Data.HashMap.Lazy                as HM
import qualified Data.HashSet                     as HS
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Text.Printf                         (printf)

import qualified Graphics.Vty                     as VT

import qualified Brick.Main                       as B
import qualified Brick.Types                      as B
import qualified Brick.Widgets.Border             as B
import qualified Brick.Widgets.Core               as B
import qualified Brick.Widgets.Center             as B
import           Brick.Widgets.Core                  ( str
                                                     , vLimit, hLimit, vBox
                                                     )

import           Tabu.Supplementary


-- | Address family
class (Eq af, Generic af, Hashable af, Ord af, Show af) ⇒ IsAF af

-- | An address is either a point ('APt') in some space, or a vector ('AVec')
data Address af
    = APt  { aPt  ∷ af }
    | AVec { aPt  ∷ af
           , aIdx ∷ Float }
    deriving (Eq, Generic, Hashable, Show)

type family AFOf x ∷ *

data AtmF af = ∀ a. (Atom a, af ~ AFOf a) ⇒ AtmF a


deriving instance Hashable VT.Key
deriving instance Hashable VT.Modifier

data KeySpec
    = NonCharKeys
    | CharKeys
    | ModKey VT.Key [VT.Modifier]
    | Key VT.Key
    | CharKey Char
    deriving (Eq, Ord, Generic, Hashable, Show)
               -- Ord carries specificity

key_charp ∷ VT.Key → Bool
key_charp (VT.KChar '\t') = False
key_charp (VT.KChar _)    = True
key_charp _               = False

lift_modkey ∷ VT.Key → [VT.Modifier] → KeySpec
lift_modkey k@(VT.KChar '\t') []  = Key     k
lift_modkey   (VT.KChar c)    []  = CharKey c
lift_modkey k                 []  = Key     k
lift_modkey k            ms@(_:_) = ModKey  k ms


class (IsAF (AFOf mol), mol ~ Mol (AFOf mol)) ⇒ Molecule mol a | mol → a

data                          MolF =   ∀ mol a. Molecule mol a ⇒ MolF mol

type instance AFOf (Mol x) = x

newtype Name = Name String deriving (Eq, Ord, Show)

data Mol af where
     Mol ∷ IsAF af ⇒
          { molecule_title          ∷ String
          , molecule_event_route    ∷ MolEventRoute af
          , molecule_map            ∷ MolAtomMap af
          , molecule_bind_descr     ∷ [(KeySpec, String)]
          , molecule_focus          ∷ Address af
          , molecule_key_prev       ∷ VT.Key                    -- Lol, AG as AtmF?
          , molecule_key_next       ∷ VT.Key
          , molecule_popup          ∷ Maybe (String, [String])
          , molecule_needs_viewport ∷ Bool
          , _molecule_log           ∷ [[String]] } → Mol af
type MolEventRoute af = HM.HashMap KeySpec (HS.HashSet (Address af))
type MolAtomMap    af = HM.HashMap (Address af) (AtmF af)

molecule ∷ ∀ mol a. Molecule mol a ⇒ String → (VT.Key, VT.Key) → Bool → [(Address (AFOf mol), AtmF (AFOf mol))] → MolF
molecule molecule_title (molecule_key_prev, molecule_key_next) molecule_needs_viewport elts =
    let molecule_event_route = compose_event_route elts
        molecule_map         = HM.fromList elts
        molecule_bind_descr  = compose_help $ fmap snd elts
        molecule_focus       = fst $ head elts
        molecule_popup       = Nothing
        _molecule_log        = []
    in MolF $ Mol{..}

molecule_draw ∷ ∀ mol a. Molecule mol a ⇒ mol → [B.Widget Name]
molecule_draw mol@Mol{..} =
    case molecule_popup of
         Nothing           → []
         Just (title, msg) → [ B.vCenter ∘ B.hCenter ∘ vLimit (3 + length msg) ∘ hLimit 100
                             ∘ B.borderWithLabel (str $ "  " <> title <> "  ") $ vBox $ fmap str msg ]
    <>
    [ B.vCenter ∘ B.hCenter
      ∘ B.borderWithLabel (str $ " " <> molecule_title <> "   Alt-h for help ")
      $ vBox $ molecule_render mol ]

_log,  _log_new  ∷ ∀ mol a. Molecule mol a ⇒ String → Mol (AFOf mol) → Mol (AFOf mol)
_logf, _logf_new ∷ String → MolF → MolF
_log      logstr (mol@Mol{..}) = mol { _molecule_log = (logstr : head _molecule_log) : tail _molecule_log }
_log_new  logstr (mol@Mol{..}) = mol { _molecule_log = [logstr] : _molecule_log }
_logf     logstr (MolF x)      = MolF $ _log logstr x
_logf_new logstr (MolF x)      = MolF $ _log_new logstr x


type EventM a = B.EventM Name a

class HandleEvent a where
  handleEvent ∷ B.BrickEvent Name VT.Event → a → EventM a


data Reaction af r                                         -- It's handy that this is polymorphic on the second argument.
    = Update r
    | Change r
    | Disappear r
    | Spawn Ordering (EventM r)
    | UpdateAt (Address af) r
    | PushMolecule (EventM MolF)

instance Show (Address af) ⇒ Show (Reaction af r) where
    show (Update _)       = "Update"
    show (Change _)       = "Change"
    show (Disappear _)    = "Disappear"
    show (Spawn _ _)      = "Spawn"
    show (UpdateAt a _)   = "UpdateAt " <> show a
    show (PushMolecule _) = "PushMolecule"

type Handler af atom = Mol af → B.BrickEvent Name VT.Event → atom → EventM [Reaction af (AtmF af)]

data Atm a = Atm a deriving Functor
class ( a ~ Atm (Data a, Widget a)
      , HandleEvent (Widget a)
      , Typeable a
      , IsAF (AFOf a) )
    ⇒ Atom a where
    type Data         a ∷ * -- ^ Data being represented
    type Widget       a ∷ * -- ^ Means of representation
    -- should the state be compartmentalised?
    derive_atom          ∷ Data a → a
    atom_event_filter    ∷ a → Bool → B.BrickEvent Name VT.Event → Bool
    atom_handle_event_w  ∷ B.BrickEvent Name VT.Event → a → EventM a
    atom_handlers        ∷ a → [(KeySpec, Handler (AFOf a) a, String)]
    atom_focus           ∷ a → Ordering → a
    atom_unfocus         ∷ a → a
    render_atom          ∷ Bool → a → B.Widget Name
    --
    atom_event_filter      _ focused _ = focused
    atom_handle_event_w ev (Atm (d, w)) = Atm ∘ (d,) <$> handleEvent ev w
    atom_focus                         = const
    atom_unfocus                       = id


-- * Secondary: instances
instance Ord af ⇒ Ord (Address af) where
    compare a b = let on_pt = (compare `on` aPt) a b
                  in case on_pt of
                       EQ → case (a, b) of
                              (APt _,    APt _)    → EQ
                              (APt _,    AVec _ _) → LT
                              (AVec _ _, APt _)    → GT
                              (AVec _ _, AVec _ _) → (compare `on` aIdx) a b
                       _  → on_pt
                  -- Something tells me it can be done much simpler..

instance Functor (Reaction af) where
    fmap f (Update a)          = Update $ f a
    fmap f (Change a)          = Change $ f a
    fmap f (Disappear a)       = Disappear $ f a
    fmap f (UpdateAt addr a)   = UpdateAt addr $ f a
    fmap f (Spawn o ma)        = Spawn o $ fmap f ma
    fmap _ (PushMolecule m)    = PushMolecule m

(<||>) ∷ [Reaction af r] → EventM [Reaction af r] → EventM [Reaction af r]
x <||> ya = case x of
              [] → ya
              _  → pure x


-- *  Molecule
addr_order_pair ∷ IsAF af ⇒ Address af → Address af → (Address af, Address af)
addr_order_pair x y | x >= y = (x, y)
addr_order_pair x y          = (y, x)

-- XXX:  non-total: ⊥ on non-neighbours
addr_new_between_neighbours ∷ IsAF af ⇒ (Address af, Address af) → Maybe (Address af)
addr_new_between_neighbours (AVec x _, AVec y _) | x ≢ y = Nothing
addr_new_between_neighbours (APt  _,   APt  _)           = Nothing
addr_new_between_neighbours (AVec p x, APt  _)           = Just $ AVec p (succ x)
addr_new_between_neighbours (APt  _,   AVec p y)         = Just $ AVec p (pred y)
addr_new_between_neighbours (AVec p x, AVec _ y)         = Just $ AVec p ((x + y) / 2)

compose_event_route ∷ [(Address af, AtmF af)] → MolEventRoute af
compose_event_route                              xs = foldl superimpose_atom_kbs HM.empty xs
    where superimpose_atom_kbs ∷ MolEventRoute af → (Address af, AtmF af) → MolEventRoute af
          superimpose_atom_kbs route (addr, AtmF a) = foldl (superimpose_binding addr) route ∘ map (^. _1) $ atom_handlers a
          superimpose_binding addr route ks         = HM.unionWith HS.union route (HM.fromList [(ks, HS.singleton addr)])

molecule_recompose_event_route ∷ Mol af → Mol af
molecule_recompose_event_route mol@Mol{..} =
    mol { molecule_event_route = compose_event_route $ HM.toList molecule_map }

compose_help ∷ [AtmF af] → [(KeySpec, String)]
compose_help xs = foldl recur [] xs
    where recur ∷ [(KeySpec, String)] → AtmF af → [(KeySpec, String)]
          recur helps (AtmF a) = foldl (<>) helps ∘ map (\(ks, _, kshelp) → [(ks, kshelp)]) $ atom_handlers a

molecule_handling_addrs ∷ Mol af → KeySpec → HS.HashSet (Address af)
molecule_handling_addrs Mol{..} k = fromMaybe HS.empty $ HM.lookup k molecule_event_route

molecule_atom_at ∷ IsAF af ⇒ Mol af → Address af → AtmF af
molecule_atom_at Mol{..} addr = flip fromMaybe (HM.lookup addr molecule_map)
                              $ error $ printf "No AtmF at address '%s'." (show addr)

atom_try_handle_event ∷ Mol af → B.BrickEvent Name VT.Event → Bool → KeySpec → AtmF af → EventM [Reaction af (AtmF af)]
atom_try_handle_event mol ev focused kspec (AtmF a) =
    if atom_event_filter a focused ev
    then do let handler = (flip fromMaybe) ((^. _2) <$> (find ((≡ kspec) ∘ (^. _1)) $ atom_handlers a))
                          $ error $ printf "Internal invariant failed:  AtmF has no binding for key, whereas by 'molecule_handling_addr' it should."
            handler mol ev a
    else pure []

molecule_update_addr ∷ IsAF af ⇒ Address af → AtmF af → Mol af → Mol af
molecule_update_addr addr atom mol@Mol{..} = mol { molecule_map = HM.insert addr atom molecule_map }

molecule_clear_addr ∷ IsAF af ⇒ Address af → Mol af → Mol af
molecule_clear_addr addr mol@Mol{..} = mol { molecule_map = HM.delete addr molecule_map }

molecule_set_focus ∷ Address af → Mol af → Mol af
molecule_set_focus addr mol = mol { molecule_focus = addr }

molecule_try_handle_event_with_keyspec_at ∷ B.BrickEvent Name VT.Event → KeySpec → Address af → Mol af → EventM [Reaction af (AtmF af)]
molecule_try_handle_event_with_keyspec_at ev kspec addr mol@Mol{..} =
    case HM.lookup addr molecule_map of
      Just atom → atom_try_handle_event mol ev (molecule_focus ≡ addr) kspec atom
      Nothing   → error $ printf "Asked to handle event at unoccupied address %s." (show addr)

molecule_compute_address_line ∷ Ord af ⇒ Mol af → [Address af]
molecule_compute_address_line Mol{..} = sort $ HM.keys molecule_map

molecule_neighbor ∷ IsAF af ⇒ Ordering → Address af → Mol af → Address af
molecule_neighbor order addr mol@Mol{..} =
    let addrline = molecule_compute_address_line mol
    in flip fromMaybe (ring_neighbor (≡ addr) order addrline)
       $ error $ printf "Asked to insert a neighbor, relative to a non-existent address %s." (show addr)

molecule_insert_relative ∷ IsAF af ⇒ Address af → Ordering → AtmF af → Mol af → (Address af, Mol af)
molecule_insert_relative addr order atom mol@Mol{..} =
    let neigh_addr = molecule_neighbor order addr mol
        new_addr   = flip fromMaybe (addr_new_between_neighbours $ addr_order_pair addr neigh_addr)
                     $ error $ printf "Failed to insert a new address between %s and %s." (show addr) (show neigh_addr)
    in (new_addr, molecule_update_addr new_addr atom mol)

molecule_defocus ∷ Molecule mol a ⇒ Mol (AFOf mol) → Mol (AFOf mol)
molecule_defocus mol@Mol{..} =
    mol { molecule_focus = molecule_neighbor LT molecule_focus mol }

molecule_remove_atom ∷ Molecule mol a ⇒ Address (AFOf mol) → Mol (AFOf mol) → Mol (AFOf mol)
molecule_remove_atom addr mol =
    molecule_recompose_event_route $ molecule_clear_addr addr $ molecule_defocus mol

molecule_help ∷ MolF → [String]
molecule_help (MolF Mol{..}) =
    [ printf "%-20s: %s" (show ks) desc
    | (ks, desc) ← molecule_bind_descr ]

data Effect mol a where
    ReplaceM  ∷ Mol (AFOf mol) → Effect mol a
    PushM     ∷ MolF →           Effect mol a
    PopM      ∷                  Effect mol a

data GlobalEffect where
    ReplaceMF ∷ MolF           → GlobalEffect
    Quit      ∷                  GlobalEffect

instance Show (Effect mol a) where
    show PopM         = "PopM"
    show (ReplaceM _) = "ReplaceM"
    show (PushM _)    = "PushM"

molecule_effect_of_reaction ∷ Molecule mol a ⇒ Reaction (AFOf mol) (Address (AFOf mol), AtmF (AFOf mol)) → Mol (AFOf mol) → EventM (Effect mol a)
molecule_effect_of_reaction (Update        (srcaddr, atom)) m = pure ∘ ReplaceM $ molecule_update_addr srcaddr atom m
molecule_effect_of_reaction (Change        (srcaddr, atom)) m = pure ∘ ReplaceM $ molecule_recompose_event_route $ molecule_update_addr srcaddr atom m
molecule_effect_of_reaction (Disappear     (srcaddr, _))    m = pure ∘ ReplaceM $ molecule_remove_atom srcaddr m
molecule_effect_of_reaction (UpdateAt addr (_,       atom)) m = pure ∘ ReplaceM $ molecule_update_addr addr atom m
molecule_effect_of_reaction (Spawn ord action')             m = do
  (srcaddr, atom) ← action'
  let (spawn_addr, m') = molecule_insert_relative srcaddr ord atom m
  pure ∘ ReplaceM $ molecule_set_focus spawn_addr $ molecule_recompose_event_route m'
molecule_effect_of_reaction (PushMolecule m)                _ = PushM <$> m

molecules_apply_effect ∷ Molecule mol a ⇒ Mol (AFOf mol) → [MolF] → Effect mol a → (Maybe (Mol (AFOf mol)), [MolF])
molecules_apply_effect m ms eff =
    case eff of
      PopM        → (Nothing, ms)
      ReplaceM m' → (Just m', ms)
      PushM    m' → (Nothing, m' : MolF m : ms)

molecules_apply_reactions ∷ ∀ mol a. Molecule mol a ⇒ Mol (AFOf mol) → [MolF] → [Reaction (AFOf mol) (Address (AFOf mol), AtmF (AFOf mol))] → EventM [MolF]
molecules_apply_reactions m ms [] = pure $ MolF m : ms
molecules_apply_reactions m ms (r:rs) = do
  eff ← molecule_effect_of_reaction r m
  let (mmol, ms') = molecules_apply_effect m ms eff
  case (mmol, rs) of
    (Nothing, [])  → pure ms'
    (Nothing, _:_) → error "Bad sequence of reactions: reactions follow stack modification."
    (Just m', rs') → molecules_apply_reactions m' ms' rs'

global_keybinding_effects ∷ KeySpec → [MolF] → Maybe GlobalEffect
global_keybinding_effects (Key VT.KEsc) []                                               = Just Quit
global_keybinding_effects (Key VT.KEsc) (MolF   Mol{..} : []) | Nothing ← molecule_popup = Just Quit
global_keybinding_effects (Key VT.KEsc) (MolF m@Mol{..} : _)  | Just _  ← molecule_popup = Just ∘ ReplaceMF ∘ MolF $ m { molecule_popup = Nothing }
global_keybinding_effects _             _                                                = Nothing

molecules_handle_weak_global_keybindings ∷ B.BrickEvent Name VT.Event → [MolF] → [MolF]
molecules_handle_weak_global_keybindings (B.VtyEvent (VT.EvKey k mods)) ms@(molf@(MolF mol@Mol{..}) : rest) =
    if | k ≡ VT.KChar 'h' ∧ mods ≡ [VT.MMeta]
           → (:rest) $ molecule_set_popup "Key bindings:" molf $ molecule_help molf
       | k ≡ molecule_key_prev ∨ k ≡ molecule_key_next
           → let addrline         = molecule_compute_address_line mol
                 (dfocus, newufa) = if | k ≡ molecule_key_prev → (LT, last $ [last addrline] <> takeWhile (≢ molecule_focus) addrline)
                                       | k ≡ molecule_key_next → (GT, head $ drop 1 $ dropWhile (≢ molecule_focus) addrline <> [head addrline])
             in case (HM.lookup molecule_focus molecule_map, HM.lookup newufa molecule_map) of
                  (Nothing, _) → error $ printf "Nothing at new focus address: %s." (show molecule_focus)
                  (_, Nothing) → error $ printf "Nothing at previous focus address: %s." (show newufa)
                  (Just (AtmF oldf), Just (AtmF newuf)) →
                      let (olduf, newf) = ( AtmF $ atom_unfocus oldf
                                          , AtmF $ atom_focus newuf dfocus )
                      in (:rest) $ MolF mol { molecule_focus = newufa
                                            , molecule_map   = HM.fromList [(molecule_focus, olduf), (newufa, newf)] `HM.union` molecule_map }
       | k ≡ VT.KEsc
           → rest
       | otherwise
           → ms
molecules_handle_weak_global_keybindings _ev mols = mols

molecules_handle_event ∷ [MolF] → B.BrickEvent Name VT.Event → EventM (B.Next [MolF])
molecules_handle_event mols@(MolF mol@Mol{..}:_) ev@(B.VtyEvent (VT.EvKey k ms)) = do
  let try_handle_by_addr kspec addr acc = do
        reply ← molecule_try_handle_event_with_keyspec_at ev kspec addr mol
        case reply of
          []      → acc
          handled → pure $ map (fmap (addr,)) handled
      try_kspec kspec =                              -- XXX: the per-keybinding handler precedence is arbitrary
          foldr (try_handle_by_addr kspec) (pure []) $ HS.toList $ molecule_handling_addrs mol kspec
      direct_kspec = lift_modkey k ms
  case global_keybinding_effects direct_kspec mols of
    Just Quit             → B.halt []
    Just (ReplaceMF molf) → B.continue $ molf : tail mols
    Nothing               → do
      rs  ←          try_kspec $ direct_kspec
      rs' ← rs <||> (try_kspec $ if key_charp k ∧ ms ≡ []
                                 then CharKeys
                                 else NonCharKeys)
      case rs' of
        []   → B.continue $ molecules_handle_weak_global_keybindings ev mols -- no handler claimed the event
        rs'' → B.continue =<< molecules_apply_reactions mol (tail mols) rs''
molecules_handle_event (_:_) _ = error "molecules_handle_event: non-key event passed."
molecules_handle_event []    _ = error "molecules_handle_event: empty molecule stack."

molecule_atoms ∷ Mol af → [(Address af, AtmF af)]
molecule_atoms Mol{..} = HM.toList molecule_map

molecule_set_popup ∷ String → MolF → [String] → MolF
molecule_set_popup title (MolF mol) msg = MolF mol { molecule_popup = Just (title, msg) }

molecule_render ∷ IsAF af ⇒ Mol af → [B.Widget Name]
molecule_render mol@Mol{..} =
    -- map (str ∘ intercalate ", " ∘ reverse) (take 5 $ _molecule_log) <> [str "---"] <>
    [ maybe_with_viewport
      $ vBox
      $ map (\(faddr, AtmF atom) →
             (if faddr ≡ molecule_focus then B.visible else id)
             $ render_atom (faddr ≡ molecule_focus) atom)
      $ sortBy (compare `on` fst)
      $ molecule_atoms mol ]
    where
      maybe_with_viewport | molecule_needs_viewport = B.viewport (Name $ "MoleculeViewport" <> molecule_title) B.Vertical
                          | otherwise               = id

with_molecule_atom ∷ ∀ a af res. (Atom a, IsAF af) ⇒ Mol af → Address af → (a → res) → res
with_molecule_atom mol addr fn =
    case molecule_atom_at mol addr of
      AtmF atom →
          case cast atom ∷ Maybe a of
            Nothing → error $ printf "Invariant failed: expected atom not found at address %s, party is over.." (show addr)
            Just a  → fn a
