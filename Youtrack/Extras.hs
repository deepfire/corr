{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UnicodeSyntax #-}

module Youtrack.Extras
where

import qualified Data.HashMap.Lazy                as HM
import           Data.Maybe
import           Data.Monoid
import           Prelude.Unicode
import           Text.Printf                         (printf)
import           System.IO.Unsafe                    (unsafeInterleaveIO)


-- Local imports
import           YTColumns
import           Youtrack                     hiding (Field, State, Tag)
import qualified Youtrack                         as Y


yt_issues ∷ YT → ProjectDict → Filter → Int → IO [Issue]
yt_issues yt pdict ifilter ilimit = unsafeInterleaveIO $ do
  ytRequest yt pdict ∘ RIssue ifilter ilimit ∘ (Y.Field "projectShortName":)
  $ mapMaybe field_yt_fieldid all_fields

project_issue ∷ Project → IId → IO (Maybe Issue)
project_issue p@Project{..} iid = do
  listToMaybe <$> yt_issues project_yt (HM.singleton project_alias p) (Filter $ "#" <> palias_iid_idstr project_alias iid) 1

issue_comments ∷ Issue → IO [Comment]
issue_comments i@Issue{..} | Project {..} ← issue_project = do
  projectRequest issue_project i $ RIssueComments project_alias issue_id

issue_update_description ∷ Issue → Description → IO ()
issue_update_description Issue{..} val | Project {..} ← issue_project = do
  ytRequestRaw project_yt $ RIssueUpdate project_alias issue_id issue_summary val
  >> pure ()

-- XXX:  another abstraction layer awaits: YTCmd
issue_execute ∷ Issue → YTCmd → IO ()
issue_execute Issue{..} cmd | Project {..} ← issue_project = do
  ytRequestRaw project_yt $ RIssueExecute project_alias issue_id cmd True
  >> pure ()

issue_post_comment ∷ Issue → Comment → IO ()
issue_post_comment Issue{..} Comment{..} | Project {..} ← issue_project = do
  ytRequestRaw project_yt $ RIssuePostComment project_alias issue_id comm_text
  >> pure ()

error_no_project_access ∷ Project → String → a
error_no_project_access Project{..} desc =
    error $ printf "Requested to %s, but the logon user '%s' has no access to project '%s'."
          desc (show $ fromMLogin $ ytLogin project_yt) (fromPAlias project_alias)

the_project_accessor_for ∷ Project → String → Member
the_project_accessor_for p@Project{..} desc | Just accessor ← project_accessor = accessor
                                            | otherwise = error_no_project_access p desc
