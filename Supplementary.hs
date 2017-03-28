{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE SignatureSections #-} 8.2
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE UndecidableSuperClasses #-} 8.0
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Supplementary
where

import           Control.Exception
import qualified Control.Exception           as EX
import           Data.Char                      (toTitle)
import           Data.Data
import           Data.Default                   (Default(..))
import           Data.List
import           Data.Monoid
import           Data.Time.Calendar             (Day(..))
import           Data.Time.LocalTime            (LocalTime(..), TimeOfDay(..))
import           Debug.Trace                    (trace)
import           Test.QuickCheck
import qualified Text.Printf as P
import           Prelude.Unicode

import           Youtrack


-- * Generic tools

map_ring_point ∷ (a → Bool) → (a → a → a → b) → [a] → Maybe b
map_ring_point _      _      []   = Nothing
map_ring_point finder mapper ring = step_ring (last ring) ring
    where step_ring left (this : []) = if not $ finder this then Nothing
                                       else Just $ mapper left this (head ring)
          step_ring left (this : rest@(right : _)) =
              if not $ finder this then step_ring this rest
              else Just $ mapper left this right
          step_ring _    [] = error "map_ring_point:  implementation error: step_rint passed an empty list"

ring_neighbor ∷ (a → Bool) → Ordering → [a] → Maybe a
ring_neighbor finder spec =
    map_ring_point finder (\l t r → case spec of
                                      LT → l
                                      EQ → t
                                      GT → r)

infixr 9 >?->

alternately, (>?->) ∷ Monad m ⇒ Maybe a → m (Maybe a) → m (Maybe a)
alternately Nothing action = action
alternately x       _      = pure x

(>?->) = alternately

map1st ∷ (a → a) → [a] → [a]
map1st _ []     = []
map1st f (x:xs) = f x : xs

-- | 'detailedFail': wrap an IO computation (main), so that any exceptions raised
-- during its execution are displayed in a way that attributes them to a module.
detailedFail ∷ IO a → IO a
detailedFail ioaction =
    EX.catch ioaction $
          \(SomeException e ∷ SomeException) →
              let typerep   = typeOf e
                  tycon     = typeRepTyCon typerep
                  exmodule  = tyConModule tycon
                  expackage = tyConPackage tycon
              in throw $ trace (P.printf "FATAL: uncaught exception %s.%s/%s from %s:"
                                exmodule (show typerep) (show tycon) (show expackage))
                      e

either_exception_or ∷ IO a → IO (Either SomeException a)
either_exception_or ioaction =
    let handler (e ∷ SomeException) = pure $ Left e
    in EX.handle handler $ fmap Right $ evaluate =<< ioaction

hayfindrest ∷ String → String → Maybe String
hayfindrest haystack needle = fmap ((flip drop) haystack ∘ (+ (length needle))) $ findIndex (isPrefixOf needle) $ tails haystack

strHasTokens ∷ [String] → String → Bool
strHasTokens []       _       = True
strHasTokens (tok:xs) leltstr = case hayfindrest leltstr tok of
                                  Just leltail → strHasTokens xs leltail
                                  Nothing      → False

show_unquoted, show_squoted ∷ String → String → String
show_unquoted prefix s = prefix <> ":"  <> s
show_squoted  prefix s = prefix <> ":'" <> s <> "'"


-- * Noisy instance code that was moved out of the way

instance Arbitrary Day        where arbitrary = ModifiedJulianDay <$> choose (25000, 55000)
instance Arbitrary TimeOfDay  where arbitrary = TimeOfDay  <$> arbitrary <*> arbitrary <*> arbitrary

instance Default LocalTime    where def       = LocalTime (ModifiedJulianDay 0) (TimeOfDay 0 0 0)
instance Arbitrary LocalTime  where arbitrary = LocalTime  <$> arbitrary <*> arbitrary

instance Default   FamilyName where def       = FamilyName "Invisible"

instance Default   GivenName  where def       = GivenName "joe"

instance Default   FullName   where def       = FullName def def

instance Default   MLogin     where def       = MLogin "anonymous"
instance Arbitrary MLogin     where arbitrary = MLogin <$> arbitrary_loweralpha
instance Default   MFullName  where def       = MFullName def
instance Arbitrary MFullName  where arbitrary = MFullName <$> arbitrary

instance Default   Member     where def       = Member def def
instance Arbitrary Member     where arbitrary = Member <$> arbitrary <*> arbitrary


arbitrary_alphanum      ∷ Gen String
arbitrary_alphanum      = listOf $ frequency [ (7,  elements ['A'..'Z'])
                                             , (26, elements ['a'..'z'])
                                             , (10, elements ['0'..'9']) ]
arbitrary_hexadecimal   ∷ Gen String
arbitrary_hexadecimal   = listOf $ frequency [ (6,  elements ['a'..'g'])
                                             , (10, elements ['0'..'9']) ]
arbitrary_loweralpha    ∷ Gen String
arbitrary_loweralpha    = listOf $ elements  ['a'..'z']
arbitrary_sentence      ∷ Gen String
arbitrary_sentence      = fmap ((map1st toTitle) ∘ unwords) ∘ listOf $ arbitrary_loweralpha
arbitrary_refname       ∷ Gen String
arbitrary_refname       = fmap (take 27 ∘ intercalate "-") ∘ listOf $ arbitrary_loweralpha
arbitrary_capitalised   ∷ Gen String
arbitrary_capitalised   = fmap (map1st toTitle) $ arbitrary_loweralpha
