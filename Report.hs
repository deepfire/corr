{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Report
    ( issue_report
    )
where


-- Base imports
import           GHC.Generics                        (Generic)
import           Prelude.Unicode
import           Text.Printf                         (printf)


-- External imports
import qualified Data.Csv                         as CSV
import           Data.List                           (sortBy)
import           Data.Maybe                          (fromMaybe)
import           Data.Time.Calendar                  (toGregorian)
import           Data.Time.LocalTime                 (LocalTime(..))


-- Local imports
import           Youtrack                     hiding (Field, State, Tag)


instance CSV.ToField LocalTime where toField lt = CSV.toField $ show lt

data ReportDeliveredWorkItem where
    ReportDeliveredWorkItem ∷
     { _rdwi_id             ∷ String         -- issue         _id                  ;+
     , _rdwi_performed_date ∷ String         -- workitem      _date                ;+
     , _rdwi_performed_by   ∷ String         -- workitem      _author              ;+
     , rdwi_completed_work  ∷ Int            -- workitem      _duration            ;+
     } → ReportDeliveredWorkItem
    deriving (Generic, Show)
instance CSV.ToNamedRecord  ReportDeliveredWorkItem
instance CSV.DefaultOrdered ReportDeliveredWorkItem

data ReportDeliveredIssue where
    ReportDeliveredIssue ∷
     { _rdi_id             ∷ String          -- issue         _id                  ;+
     , _rdi_type           ∷ String          -- issue         _Type                ;+
     , _rdi_created_date   ∷ String          -- issue         _created             ;+
     , _rdi_created_by     ∷ String -- Full! -- issue         _reporterName        ;+
     , _rdi_start_date     ∷ String          -- 1st workitem  _date                ;i
     , _rdi_closed_date    ∷ String          -- issue         _resolved            ;+
     , _rdi_release_date   ∷ Maybe LocalTime -- issue         ???                  ;c?
     , _rdi_title          ∷ String          -- issue         _summary             ;+
     , _rdi_state          ∷ String          -- params        report_resolvedstate ;+
     , _rdi_area_path      ∷ String          -- params        project_areapath     ;+
     , _rdi_product        ∷ String          -- params        project_product      ;+
     , _rdi_estimation     ∷ Maybe Int       -- issue         ???                  ;r?
     , _rdi_completed_work ∷ Int             -- workitem      _duration            ;+
     , _rdi_feature_id     ∷ Maybe String    -- issue         ???                  ;c?
     } → ReportDeliveredIssue
    deriving (Generic, Show)
instance CSV.ToNamedRecord  ReportDeliveredIssue
instance CSV.DefaultOrdered ReportDeliveredIssue

print_localtime_date ∷ LocalTime → String
print_localtime_date (toGregorian ∘ localDay → (y, m, d)) =
    printf "%02d.%02d.%04d" d m y

issue_report ∷ String
             → String
             → Project
             → Issue
             → [WorkItem]
             → (,) ReportDeliveredIssue [ReportDeliveredWorkItem]
issue_report project_areapath project_product Project{..} Issue{..} wis =
    let wis_sorted = sortBy (\x y → compare (workitem_date x) (workitem_date y))
                     wis
        report_wis = [ ReportDeliveredWorkItem
                       (palias_iid_idstr project_alias issue_id)
                       (print_localtime_date date)
                       (printFullNameEastern $ fromMFullName $ member_fullname author)
                       (fromHours dur)
                     | WorkItem _ _ date author dur _
                         ← wis_sorted ]
        started_date = case wis_sorted of
                         x:_ → workitem_date x
                         []  → issue_created
    in (,)
       (ReportDeliveredIssue
        (palias_iid_idstr project_alias issue_id)
        (fromType issue_type)
        (print_localtime_date issue_created)
        (printFullNameEastern $ fromMFullName $ member_fullname issue_author)
        (print_localtime_date started_date)
        (print_localtime_date $ fromMaybe (error $ printf "ERROR: invariant failed: unresolved issue #%s in response."
                                          $ fromIId issue_id)
                               issue_resolved)
        Nothing                                          -- release
        (fromSummary issue_summary)
        (fromState issue_state)
        project_areapath
        project_product
        Nothing                                          -- estimation
        (sum $ map rdwi_completed_work report_wis)
        Nothing)
       report_wis
