{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Tabu.Columns where

import           Data.Data


-- * Column structure
data  ColumnInfo
    = ColumnInfo
      { colinfo_desc       ∷ String
      , colinfo_width      ∷ Int
      , colinfo_order      ∷ Ordering
      }

class (Show idx, Show (Field idx), Typeable idx, Ord (Field idx)) ⇒ Column idx a | idx → a where
    type Field idx     ∷ *
    column_info        ∷ idx → ColumnInfo
    render_field'      ∷ idx → a → String

