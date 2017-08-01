{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Issue where


-- * Non-local imports

import qualified Data.Aeson            as AE
import           Data.Char                (toUpper)
import           Data.Default
import qualified Data.HashMap.Lazy     as HM
import           Data.String              (IsString(..))
import qualified Data.Text             as T
import qualified Data.Vector           as V
import           Prelude.Unicode
import           Test.QuickCheck


import           Tabu.Supplementary


-- * Local imports
import           Git
import           Youtrack          hiding (Field, State, Tag)
import qualified Youtrack              as Y


-- * Instances
instance Arbitrary PAlias      where arbitrary  = PAlias      <$> fmap (fmap toUpper) arbitrary_loweralpha
instance Arbitrary PName       where arbitrary  = PName       <$> arbitrary_capitalised
instance Arbitrary Summary     where arbitrary  = Summary     <$> arbitrary_sentence
instance Arbitrary Description where arbitrary  = Description <$> arbitrary_sentence

instance Default   MLogin      where def        = MLogin "anonymous"
instance Arbitrary MLogin      where arbitrary  = MLogin <$> arbitrary_loweralpha
instance Default   MFullName   where def        = MFullName def
instance Arbitrary MFullName   where arbitrary  = MFullName <$> arbitrary
instance Default   Member      where def        = Member def def
instance Arbitrary Member      where arbitrary  = Member <$> arbitrary <*> arbitrary
instance Default   Summary     where def        = Summary     "<no-summary>"
instance Default   Description where def        = Description "<no-description>"
instance Show      Summary     where show       = show_squoted "summary"     ∘ fromSummary
instance Show      Description where show       = show_squoted "description" ∘ fromDescription
instance Show      MFullName   where show       = show_squoted "fullname"    ∘ printFullNameEastern ∘ fromMFullName

instance Show      Type        where show       = fromType
instance Show      Priority    where show       = fromPriority
instance Show      Hours       where show       = show ∘ fromHours

instance IsString  Filter      where fromString = Filter
instance IsString  Y.Field     where fromString = Y.Field

instance Arbitrary Project where
    arbitrary = Project <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

mkMissingIssue ∷ Project → RefName → Issue
mkMissingIssue p (RefName branchname) =
    Issue { issue_project     = p
          , issue_id          = IId 0
          , issue_summary     = Summary ""
          , issue_type        = Type ""
          , issue_priority    = Priority ""
          , issue_author      = def
          , issue_assignee    = def
          , issue_state       = Y.State ""
          , issue_created     = def
          , issue_resolved    = Nothing
          , issue_updated     = def
          , issue_estimation  = Nothing
          , issue_description = Nothing
          , issue_votes       = 0
          , issue_links       = []
          , issue_tags        = []
          , issue_fields      = HM.fromList
                           [("Branch", AE.Array $ V.fromList [AE.String $ T.pack branchname])
                           ,("WG",     AE.Array $ V.empty)] }
instance Arbitrary Issue where
    arbitrary = do
      issue_project     ← arbitrary
      issue_id          ← IId <$> arbitrary
      issue_summary     ← Summary <$> arbitrary_sentence
      issue_type        ← Type <$> arbitrary_capitalised
      issue_priority    ← Priority <$> arbitrary_capitalised
      issue_author      ← arbitrary
      issue_assignee    ← arbitrary
      issue_state       ← Y.State <$> arbitrary_capitalised
      issue_created     ← arbitrary
      issue_resolved    ← pure Nothing
      issue_updated     ← arbitrary
      issue_estimation  ← pure Nothing
      issue_description ← pure Nothing
      issue_votes       ← pure 0
      issue_links       ← pure []
      issue_tags        ← pure []
      issue_fields      ← let arbAEobj name os =
                                  (name, AE.Array $ V.fromList $ fmap (AE.String ∘ T.pack) os)
                          in do
                            bname   ← arbitrary_refname
                            wgnames ← listOf arbitrary_capitalised
                            pure $ HM.fromList [ arbAEobj "Branch" [bname]
                                               , arbAEobj "WG"     wgnames ]
      pure Issue{..}
