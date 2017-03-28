{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module Columns where

import           GHC.Generics               (Generic)
import           Prelude.Unicode

import qualified Data.Aeson              as AE
import           Data.Data
import           Data.Default
import           Data.Function
import           Data.Hashable              (Hashable)
import qualified Data.HashMap.Lazy       as HM
import           Data.List
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text               as T
import           Data.Time.Format           (formatTime, defaultTimeLocale)
import           Data.Time.LocalTime        (LocalTime(..))
import qualified Data.Vector             as V

import           Test.QuickCheck

import           Text.Read

import           Git
import           Supplementary
import           Work
import           Youtrack            hiding (Field, State)
import qualified Youtrack                as Y


-- * Column structure
data  ColumnInfo
    = ColumnInfo
      { colinfo_desc       ∷ String
      , colinfo_width      ∷ Int
      , colinfo_order      ∷ Ordering
      , colinfo_yt_fieldid ∷ Maybe Y.Field }

class (Show idx, Show (Field idx), Typeable idx, Ord (Field idx)) ⇒ Column idx where
    type Field idx     ∷ *
    column_info        ∷ idx → ColumnInfo
    render_field'      ∷ idx → Work → String
    work_field         ∷ idx → Work → Field idx
    issue_field        ∷ idx → Issue → Maybe (Field idx)
    issue_field    _ _ = Nothing

data WSColumn where
    WSC ∷ ∀ idx. Column idx ⇒ { unwrap ∷ idx } → WSColumn

field_name                  ∷ WSColumn → String
field_name          WSC{..} = colinfo_desc $ column_info unwrap
field_width_specifier       ∷ WSColumn → Int
field_width_specifier WSC{..} = colinfo_width $ column_info unwrap
field_default_order         ∷ WSColumn → Ordering
field_default_order WSC{..} = colinfo_order $ column_info unwrap
field_yt_fieldid            ∷ WSColumn → Maybe Y.Field
field_yt_fieldid    WSC{..} = colinfo_yt_fieldid $ column_info unwrap

instance Eq WSColumn where
    (==) = (≡) `on` (\(WSC x) → typeOf x)


-- * Branch fields

data TBranch     = SBranch    deriving (Show)
instance Column    TBranch    where
    type Field     TBranch        = RefName
    column_info    SBranch        = ColumnInfo "Branch" 32 GT (Just "Branch")
    render_field'  SBranch        = fromRefName ∘ work_refname
    work_field     SBranch        = work_refname


data TCommitted  = SCommitted deriving (Show)
instance Column    TCommitted where
    type Field     TCommitted     = LocalTime
    column_info    SCommitted     = ColumnInfo "Committed" 13 LT Nothing
    render_field'  SCommitted     = formatTime defaultTimeLocale "%d %b  %H:%M" ∘ committed ∘ work_branch
    work_field     SCommitted     = committed ∘ work_branch


data TCommitter  = SCommitter deriving (Show)
instance Column    TCommitter where
    type Field     TCommitter     = String
    column_info    SCommitter     = ColumnInfo "Committer" 18 GT Nothing
    render_field'  SCommitter     = committer ∘ work_branch
    work_field     SCommitter     = committer ∘ work_branch


-- * Issue fields

data TIssue      = SIssue     deriving (Show)
instance Column    TIssue     where
    type Field     TIssue         = Int
    column_info    SIssue         = ColumnInfo "Issue ID" (-8) GT (Just "numberInProject")
    render_field'  SIssue       w = let iid = work_field SIssue w
                                    in if | iid ≡ 0     → ""
                                          | otherwise   → issue_idstr $ work_issue w
    work_field     SIssue         = fromMaybe (0 ∷ Int) ∘ issue_field SIssue ∘ work_issue
    issue_field    SIssue         = Just ∘ fromIId ∘ issue_id


data TType       = SType      deriving (Show)
instance Column    TType      where
    type Field     TType          = Type
    column_info    SType          = ColumnInfo "Type" 12 GT (Just "Type")
    render_field'  SType          = fromType ∘ work_field SType
    work_field     SType          = fromMaybe (Type "No Type") ∘ issue_field SType ∘ work_issue -- How do we pass per-project defaults?
    issue_field    SType          = Just ∘ issue_type


data TSummary    = SSummary   deriving (Show)
instance Column    TSummary   where
    type Field     TSummary       = Summary
    column_info    SSummary       = ColumnInfo "Summary" 40 GT (Just "summary")
    render_field'  SSummary       = fromSummary ∘ work_field SSummary
    work_field     SSummary       = work_summary
    issue_field    SSummary       = Just ∘ issue_summary


data TDescription = SDescription deriving (Show)
instance Column    TDescription where
    type Field     TDescription   = Description
    column_info    SDescription   = ColumnInfo "Description" 40 GT (Just "description")
    render_field'  SDescription   = fromDescription ∘ work_field SDescription
    work_field     SDescription   = work_description
    issue_field    SDescription   = issue_description


data TPriority   = SPriority  deriving (Show)
instance Column    TPriority  where
    type Field     TPriority      = Priority -- XXX: factor YT using Names
    column_info    SPriority      = ColumnInfo "Priority" 18 GT (Just "Priority")
    render_field'  SPriority      = fromPriority ∘ issue_priority ∘ work_issue
    work_field     SPriority      = issue_priority ∘ work_issue
    issue_field    SPriority      = Just ∘ issue_priority


data TEstimation = SEstimation deriving (Show)
instance Column    TEstimation where
    type Field     TEstimation    = Maybe Hours
    column_info    SEstimation    = ColumnInfo "Estimation" 18 LT (Just "Estimation")
    render_field'  SEstimation    = fromMaybe "" ∘ fmap (show ∘ fromHours) ∘ issue_estimation ∘ work_issue
    work_field     SEstimation    = issue_estimation ∘ work_issue
    issue_field    SEstimation    = Just ∘ issue_estimation


data TCreated    = SCreated   deriving (Show)
instance Column    TCreated   where
    type Field     TCreated       = LocalTime
    column_info    SCreated       = ColumnInfo "Created" 18 LT (Just "created")
    render_field'  SCreated       = show ∘ issue_created ∘ work_issue
    work_field     SCreated       = issue_created ∘ work_issue
    issue_field    SCreated       = Just ∘ issue_created


data TUpdated    = SUpdated   deriving (Show)
instance Column    TUpdated   where
    type Field     TUpdated       = LocalTime
    column_info    SUpdated       = ColumnInfo "Updated" 18 LT (Just "updated")
    render_field'  SUpdated       = show ∘ issue_updated ∘ work_issue
    work_field     SUpdated       = issue_updated ∘ work_issue
    issue_field    SUpdated       = Just ∘ issue_updated


data TResolved   = SResolved  deriving (Show)
instance Column    TResolved  where
    type Field     TResolved      = Maybe LocalTime
    column_info    SResolved      = ColumnInfo "Resolved" 18 LT (Just "resolved")
    render_field'  SResolved      = show ∘ issue_resolved ∘ work_issue
    work_field     SResolved      = issue_resolved ∘ work_issue
    issue_field    SResolved      = Just ∘ issue_resolved


 -- Note [Issue-Work-Column interdependence] in Work.hs
newtype  WG      = WG { fromWG ∷ String } deriving (Eq, Generic, Hashable, Ord)
instance Default   WG          where def       = WG "unknown-wg"
instance Arbitrary WG          where arbitrary = WG <$> arbitrary_refname
instance Show      WG          where show       = show_unquoted "WG" ∘ fromWG
-- Awaiting for data types a la carte: 'compdata'..
issue_wgs ∷ Issue → [WG]
issue_wgs Issue{issue_fields} =
    case HM.lookup "WG" issue_fields of
      Nothing           → []
      Just (AE.Array v) → [ WG $ T.unpack t
                          | AE.String t ← V.toList v ] -- XXX: silently skipping non-strings
      mf → error $ "malformed issue field 'WG', not an array: " <> show mf

data TWG         = SWG        deriving (Show)
instance Column    TWG        where
    type Field     TWG            = [WG]
    column_info    SWG            = ColumnInfo "Workgroup" (-20) LT (Just "WG")
    render_field'  SWG            = intercalate " " ∘ fmap fromWG ∘ work_field SWG
    work_field     SWG            = issue_wgs ∘ work_issue
    issue_field    SWG            = Just ∘ issue_wgs


data TReporter   = SReporter  deriving (Show)
instance Column    TReporter  where
    type Field     TReporter      = MFullName -- XXX: factor YT using Names
    column_info    SReporter      = ColumnInfo "Reporter" 18 LT (Just "reporterName")
    render_field'  SReporter      = show ∘ fromMFullName ∘ work_field SReporter
    work_field     SReporter      = member_fullname ∘ issue_author ∘ work_issue


data TAssignee   = SAssignee  deriving (Show)
instance Column    TAssignee  where
    type Field     TAssignee      = [MFullName] -- XXX: factor YT using Names
    column_info    SAssignee      = ColumnInfo "Assignee" (-18) LT (Just "Assignee")
    render_field'  SAssignee      = intercalate " " ∘ fmap (show ∘ fromMFullName) ∘ work_field SAssignee
    work_field     SAssignee      = fmap member_fullname ∘ issue_assignee ∘ work_issue


data State                       -- Note [Issue-Work-Column interdependence] in Work.hs
    = Complete
    | Nightly
    | Review
    | Ready
    | InProgress
    | Stalled
    | Rejected
    | NotABug
    | Duplicate
    | Unsupported
    | Later
    | NeedRepro
    | Invalid
    | New
    --
    | Undefined
      deriving (Bounded, Enum, Eq, Ord, Read, Show)
instance Default   State where def       = Undefined
instance Arbitrary State where arbitrary = arbitraryBoundedEnum

data TState      = SState     deriving (Show)
instance Column    TState     where
    type Field     TState         = State
    column_info    SState         = ColumnInfo "State" (-11) GT (Just "State")
    render_field'  SState       w = case work_field SState w of
                                      Undefined → ""
                                      x         → show x
    work_field     SState         = fromJust ∘ issue_field SState  ∘ work_issue
    issue_field    SState         = Just ∘ fromMaybe def ∘ readMaybe ∘ fromState ∘ issue_state


-- * Field-proportional code
all_fields ∷ [WSColumn]
all_fields =
    [ WSC SBranch, WSC SCommitted, WSC SCommitter
    , WSC SIssue
    , WSC SType
    , WSC SSummary, WSC SDescription
    , WSC SPriority, WSC SEstimation
    , WSC SCreated, WSC SUpdated, WSC SResolved
    , WSC SWG, WSC SReporter, WSC SAssignee
    , WSC SState ]
