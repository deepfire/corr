{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Work
    ( Work(..)
    , lift_work
    , correlate_work
    , BranchSetKind(..), Worksets
    , kind_set
    )
where


-- Base imports
import           GHC.Generics                        (Generic)
import           Prelude.Unicode
import           Text.Printf                         (printf)


-- External imports
import           Control.Arrow                       ((&&&))
import           Data.Default
import           Data.Hashable                       (Hashable)
import qualified Data.HashMap.Lazy                as HM
import qualified Data.HashSet                     as HS
import           Data.Maybe
import           Test.QuickCheck


-- Local imports
import           Git
import           Issue
import           Youtrack                     hiding (Field, State, Tag)


-- * The Work
data  Work
    = Work
      { work_refname        ∷ RefName
      , work_summary        ∷ Summary
      , work_description    ∷ Description
      , work_branch         ∷ Branch
      , work_issue          ∷ Issue }
instance Arbitrary Work where arbitrary = Work <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

lift_work ∷ Project → Maybe Branch → Maybe Issue → Maybe RefName → Work
lift_work project mb mi mr =
    Work { work_refname       = refname
         , work_summary       = issue_summary
         , work_description   = fromMaybe def issue_description
         , work_branch        = b
         , work_issue         = i }
    where r = fromMaybe def mr
          b@Branch{..} = fromMaybe (mkMissingBranch         r) mb
          i@Issue{..}  = fromMaybe (mkMissingIssue  project r) mi

correlate_work ∷ Project → HM.HashMap RefName Branch → HM.HashMap RefName Issue → HM.HashMap RefName Work
correlate_work defproject bs iss =
  let all_involved_branches = HS.union (HS.fromList $ HM.keys bs) (HS.fromList $ HM.keys iss)
  in HM.fromList [ (id &&& lift_work (fromMaybe defproject mp) mb mi ∘ Just) workname
                 | workname ← HS.toList all_involved_branches
                 , let mb = HM.lookup workname bs
                       mi = HM.lookup workname iss
                       mp = fmap issue_project mi ]


-- * In waiting for query language: Work sets
data BranchSetKind -- XXX:  a query language will obviate this
    = GIT | YT | GITMYT | YTMGIT | GITAYT
    deriving (Bounded, Enum, Eq, Generic, Read, Show, Hashable)
instance Default BranchSetKind where def = GITAYT

type Worksets = HM.HashMap BranchSetKind [Work]

kind_set ∷ BranchSetKind → Worksets → [Work]
kind_set kind sets' =
    fromMaybe (error $ printf "unable to resolve kind '%s' in computed sets" $ show kind)
    $ HM.lookup kind sets'

{- Note [Issue-Work-Column interdependence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Issue and Work used to be completely independent from Column, but several
categories of exceptions are beginning to appear:

 - Column method usage in Work-specific functions like 'lift_work' and 'pprint_work_...'
 - non-youtrack-the-module type *definitions*, which could technically be separate from the
   Column instances, but code distance considerations make this separation undesirable.
 - Issue uses Column for visualisation (Show instance).

Column itself, though, is intrinsically bound to Work, through half of its methods.
-}
