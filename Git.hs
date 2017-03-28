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
--{-# LANGUAGE PolyKinds #-}
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

module Git where


import           GHC.Exts (Constraint)
import           GHC.Generics (Generic)
import           Prelude.Unicode

import           Control.Monad.Catch (MonadMask)
import           Control.Monad.Reader (MonadIO)
import           Control.Monad.Trans.Control (MonadBaseControl)

import           Data.Default
import           Data.Hashable (Hashable)
import qualified Data.HashMap.Lazy as HM
import           Data.List
import           Data.List.Split
import           Data.Monoid
import           Data.Tagged (Tagged (..))
import qualified Data.Text as T
import           Data.Time.LocalTime (LocalTime(..), zonedTimeToLocalTime)

import           Git.Libgit2 (lgFactory)
import           Git.Libgit2.Types
import           Git.Reference
import           Git.Repository
import qualified Git.Types as GIT

import           Test.QuickCheck

import qualified Text.Printf as P


import           Youtrack.Names
import           Supplementary


type family   GitMonad (m ∷ * → *) r ∷ Constraint
type instance GitMonad m r = (MonadIO m, MonadMask m, MonadBaseControl IO m, GIT.MonadGit r m)

newtype RemoteName = RemoteName { fromRemoteName ∷ String } deriving (Eq, Show, Generic, Hashable)
newtype RefName    = RefName    { fromRefName    ∷ String } deriving (Eq, Show, Generic, Hashable, Ord)
newtype GitOid     = GitOid     { fromGitOid     ∷ String } deriving (Eq, Show, Generic, Hashable)

instance Arbitrary RemoteName  where arbitrary = RemoteName  <$> arbitrary_loweralpha
instance Arbitrary RefName     where arbitrary = RefName     <$> arbitrary_refname
instance Arbitrary GitOid      where arbitrary = GitOid      <$> (fmap (take 40) $ arbitrary_hexadecimal)
instance Default   RefName     where def       = RefName "<no-refname>"


data Ref
    = Head RefName
    | RemoteBranch RefName RemoteName
    | Tag RefName
    | Ref [String]
    deriving Show

gitref_refname ∷ Ref → RefName
gitref_refname (Head x)           = x
gitref_refname (RemoteBranch x _) = x
gitref_refname (Tag x)            = x
gitref_refname (Ref xs)           = RefName $ last xs

gitref_refstring ∷ Ref → T.Text
gitref_refstring ref = T.pack $
    case ref of
      Head (RefName n)                        → "refs/heads/" <> n
      RemoteBranch (RefName n) (RemoteName r) → "refs/remotes/" <> r <> "/" <> n
      Tag (RefName n)                         → "refs/tags/" <> n
      Ref xs                                  → "refs/" <> intercalate "/" xs

gitrepo_refcoms ∷ (MonadBaseControl IO m, MonadMask m, MonadIO m) ⇒
     FilePath → (Ref → Bool) → m [(Ref, Maybe (GIT.Commit LgRepo))]
gitrepo_refcoms repo ref_filter =
    let normalise_ref = fmap $ (\case
                                ["heads", branch] →
                                    Head $ RefName branch
                                ["tags", branch] →
                                    Tag $ RefName branch
                                ["remotes", remote, branch] →
                                    RemoteBranch (RefName branch) (RemoteName remote)
                                ref → Ref ref)
         ∘ drop 1 ∘ splitOn "/" ∘ T.unpack
    in withRepository lgFactory repo $ do
      refs ∷ [Ref] ← fmap (filter ref_filter) $ fmap normalise_ref listReferences
      commits ← mapM gitref_tipcommit refs
      pure $ zip refs commits

gitref_tipcommit ∷ GitMonad m r ⇒ Ref → m (Maybe (GIT.Commit r))
gitref_tipcommit (Tag _) = pure Nothing -- a gitlib limitation
gitref_tipcommit ref = do
  let refstr = gitref_refstring ref
  moid ← resolveReference refstr
  case moid of
    Nothing → error $ P.printf "failed to lookup reference %s." (show refstr)
    Just r  → fmap Just $ GIT.lookupCommit (Tagged r)


data  Branch
    = Branch
      { author    ∷ String
      , committer ∷ String
      , authored  ∷ LocalTime
      , committed ∷ LocalTime
      , refname   ∷ RefName
      , oid       ∷ GitOid }
    deriving (Eq)
instance Show      Branch     where show Branch{..} = show_unquoted "branch" $ fromRefName refname
instance Default   Branch     where def       = mkMissingBranch def
instance Arbitrary Branch     where arbitrary = Branch
                                                <$> (fmap show) (arbitrary ∷ Gen FullName)
                                                <*> (fmap show) (arbitrary ∷ Gen FullName)
                                                <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

mkMissingBranch ∷ RefName → Branch
mkMissingBranch branchname =
    Branch { author    = "<unknown>"
           , committer = "<unknown>"
           , authored  = def
           , committed = def
           , refname   = branchname
           , oid       = GitOid "<unknown>" }

gitrepo_branches ∷ FilePath → (Ref → Bool) → IO (HM.HashMap RefName Branch)
gitrepo_branches repo ref_filter = do                   
  desired_refcoms ← gitrepo_refcoms repo ref_filter

  let lift_git_refcom ∷ (Ref, GIT.Commit LgRepo) → Branch
      lift_git_refcom (ref, com) =
          Branch { author    = T.unpack ∘ GIT.signatureName $ GIT.commitAuthor com
                 , committer = T.unpack ∘ GIT.signatureName $ GIT.commitCommitter com
                 , authored  = zonedTimeToLocalTime ∘ GIT.signatureWhen $ GIT.commitAuthor com
                 , committed = zonedTimeToLocalTime ∘ GIT.signatureWhen $ GIT.commitCommitter com
                 , refname   = gitref_refname ref
                 , oid       = GitOid ∘ T.unpack ∘ GIT.renderObjOid $ GIT.commitOid com }
  pure $ HM.fromList
           [ (name, lift_git_refcom (ref, com))
           | (ref@(RemoteBranch name _), Just com) ← desired_refcoms ] -- XXX: silently dropping all locally-invalid refs
