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

#if (__GLASGOW_HASKELL__ > 710)
#   define HASCALLSTACK HasCallStack =>
#else
#   define HASCALLSTACK
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
    ( main
    -- * Interactive part: for GHCi
    , readIORef
    , connect, issue, issues, comments
    -- * Connection & parameters
    , get_params, Params(..), Command(..)
    , build_correlation_context, CorrCtx(..)
    , action_handler
    -- * Various IORefs
    , params'ior, yt'ior, projects'ior
    -- * Business stuff
    , issue_wgs
    , IId(..)
    --
    , issue_execute
    )
where

-- Base imports
--import           Data.Kind                         (Type)
import           GHC.Generics                        (Generic)
import           Prelude.Unicode


-- Debug imports
import           Debug.Trace                         (trace)
import           Text.Printf                         (printf)


-- External imports
import           Control.Arrow                       ((***), (&&&))
import           Control.Exception            hiding (handle, Handler)
import qualified Control.Exception                as EX
import           Control.Monad
-- import           Control.Monad.Freer       hiding (Member)
-- import           Control.Monad.Freer.Internal
-- import qualified Control.Monad.Freer           as F
-- import qualified Control.Monad.Freer.State     as F
import qualified Data.Aeson                       as AE
import qualified Data.ByteString.Lazy.UTF8        as BL
import qualified Data.ByteString.Lazy             as BL
import qualified Data.Csv                         as CSV
import           Data.Default                        (def)
import           Data.Function                       (on)
import qualified Data.HashMap.Lazy                as HM
import           Data.IORef                          (IORef, newIORef, readIORef, writeIORef)
import           Data.List                           (find, intercalate, sortBy, isPrefixOf)
import           Data.List.Split                     (chunksOf)
import           Data.Maybe                          (fromMaybe, isNothing, isJust, fromJust)
import           Data.Monoid                         ((<>))
--import           Data.Profunctor
import qualified Data.Text                        as T
import qualified Data.Vector                      as V
import           Data.Time.Clock                     (getCurrentTime, utctDay)
import           Data.Time.Calendar                  (toGregorian)
import qualified Graphics.Vty                     as VT
import           Options.Applicative          hiding (str)
import qualified Options.Applicative              as OA
import qualified System.Environment               as SE
import           Test.QuickCheck
import           System.IO.Unsafe                    (unsafePerformIO)


-- Facilitate rapid prototyping of Exchange instances
--import           Data.Aeson                          (FromJSON(..), (.:), (.:?))
--import           Network.Wreq                        (FormParam((:=)))


-- Local imports
import           Columns
import           Git
import           Molecule
import           Report
import           Supplementary
import           UI
import           Work
import           Youtrack                     hiding (Field, State, Tag)
import           Youtrack.Extras
import qualified Youtrack                         as Y


-- * Interactive part:  for GHCi

aLL_ISSUES_PLEASe ∷ Int
aLL_ISSUES_PLEASe = 10000

connect ∷ IO (YT, ProjectDict)
connect = yt_connect =<< readIORef params'ior

issues ∷ Filter → IO [Issue]
issues filter' = do
  yt       ← readIORef yt'ior
  projects ← readIORef projects'ior
  yt_issues yt projects filter' aLL_ISSUES_PLEASe

issue ∷ PAlias → Int → IO (Maybe Issue)
issue palias iiid = do
  projects ← readIORef projects'ior
  project_issue (lookup_project projects palias) $ IId iiid

comments ∷ Issue → IO ([Comment])
comments = issue_comments


-- * WSColumn / all_fields:  Show Issue

pprint_issue_multiline ∷ [WSColumn] → Issue → String
pprint_issue_multiline fws i =
    "\n-=≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡≡=-\n" <>
    intercalate "\n"
    [ printf "%-20s " (field_name w <> ":") <> fromJust s
    | w@WSC{..} ← fws
    , let s = show <$> issue_field unwrap i
    , isJust s ]

instance Show Issue where
    show = pprint_issue_multiline all_fields


-- * Youtrack interaction
params'ior   ∷ IORef Params
yt'ior       ∷ IORef YT
projects'ior ∷ IORef ProjectDict
params'ior   = unsafePerformIO $ newIORef (error "Parameters have not yet been obtained.")
yt'ior       = unsafePerformIO $ newIORef (error "A Youtrack connection has not been established yet.")
projects'ior = unsafePerformIO $ newIORef (error "A Youtrack connection has not been established yet.")

yt_connect ∷ Params → IO (YT, ProjectDict)
yt_connect Params{..} = do
  let preyt = Y.YT { ytHostname    = param_server
                   , ytRestSuffix  = "/rest"
                   , ytLogin       = MLogin param_login
                   , ytWreqOptions = (⊥)
                   , ytJar         = (⊥) }

  yt       ← ytConnect preyt (SSLOptions "" param_server_cacert) Nothing
  projects ← ytRequest yt yt RProjectAll

  let projdict = HM.fromList [ (project_alias, p)
                             | p@Project{..} ← projects ]

  writeIORef yt'ior       yt
  writeIORef projects'ior projdict

  pure $ (yt, projdict)



-- * Configuration
data  Config a
    = Config {
        _server     ∷ a
      , _restprefix ∷ a
      , _cacert     ∷ a
      , _project    ∷ a
      , _login      ∷ a }
    deriving (Functor, Foldable, Read, Show)
instance Monoid a ⇒ Monoid (Config a) where
    mempty = Config mempty mempty mempty mempty mempty
    mappend (Config srv pref p c l) (Config srv' pref' p' c' l') = Config (srv <> srv') (pref <> pref') (p <> p') (c <> c') (l <> l')

read_config ∷ String → IO (Config (Maybe String))
read_config config_filename = do
  (evaluate ∘ read =<< readFile config_filename)
    `EX.catch`
    \(ex ∷ SomeException) →
        trace (printf "WARNING: while reading config file '%s': %s." config_filename $ show ex)
         $ pure mempty

save_config ∷ Show a ⇒ Prelude.FilePath → Config a → IO ()
save_config config_filename config = do
  writeFile config_filename (show config)
    `EX.catch`
    \(ex ∷ SomeException) →
        trace (printf "WARNING: while saving config file '%s': %s." config_filename $ show ex)
         $ pure ()


-- * Control
data Command
    = CmdUI
    | CmdExec Int String
    | CmdReport {
      project_areapath     ∷ String
    , project_product      ∷ String
    -- Report
    , report_period        ∷ Maybe String
    , report_resolvedstate ∷ Maybe String
    -- Output
    , tasks_output         ∷ Maybe FilePath
    , workitems_output     ∷ Maybe FilePath
    }
    deriving (Eq, Generic, Show)

data Params where
    Params ∷
        { param_server        ∷ String
        , param_restprefix    ∷ String
        , param_server_cacert ∷ String
        , param_project_id    ∷ String
        , param_login         ∷ String
        , param_repo          ∷ String
        , param_no_network    ∷ Bool
        , param_no_meta       ∷ Bool
        , param_verbose       ∷ Bool
        , param_cmd           ∷ Maybe Command
        } → Params
    deriving (Show)

get_params ∷ IO Params
get_params = do
  config_filename ← (<> "/.corrc") <$> SE.getEnv "HOME"
  config@(Config server' restprefix' cacert' project' login') ← read_config config_filename
  params@Params{..}
      ← customExecParser
         (prefs $ disambiguate <> showHelpOnError)
         (info
          (helper <*>
           (Params
            <$> strOption (help "Youtrack instance"
                          <> long "server"  <> metavar "HOSTNAME"   <> (fromMaybe mempty $ fmap value server'))
            <*> strOption (help "Youtrack instance REST URL prefix"
                          <> long "rest-prefix" <> metavar "PREFIX" <> (fromMaybe mempty $ fmap value restprefix'))
            <*> strOption (help "CA certificate that validates server"
                          <> long "cacert"  <> metavar "FILE"       <> (fromMaybe mempty $ fmap value cacert'))
            <*> strOption (help "YouTrack project shortname (alias)"
                          <> long "project" <> metavar "ALIAS"      <> (fromMaybe mempty $ fmap value project'))
            <*> strOption (help "YouTrack login"
                          <> long "login"   <> metavar "LOGIN"      <> (fromMaybe mempty $ fmap value login'))
            <*> strOption (help "Local git repository pathname"
                          <> long "repo"    <> metavar "DIR"        <> value ".")
            <*> switch    (help "Disable network access, work with local data"
                          <> long "no-network")
            <*> switch    (help "Disable meta-header in branch listings"
                          <> long "no-meta")
            <*> switch    (help "Enable verbose operation"
                          <> long "verbose")
            <*> optional (hsubparser
            (command "ui"
             (info
              (pure CmdUI)
              (fullDesc <> progDesc "Start an intactive text-mode UI")) <>
             command "exec"
             (info
              (CmdExec <$>
               argument OA.auto (metavar "ISSUE-ID") <*>
               argument OA.str (metavar "COMMAND"))
              (fullDesc <> progDesc "Execute a YT command for an issue")) <>
             command "report"
             (info
              (CmdReport <$>
               strOption (help "Area Path"
                         <> long "areapath"          <> metavar "PATH"      <> (fromMaybe mempty $ Nothing)) <*>
               strOption (help "Product"
                         <> long "product"           <> metavar "PRODUCT"   <> (fromMaybe mempty $ Nothing)) <*>
               (optional $
                strOption (help "Period"
                          <> long "period"           <> metavar "DATE"))  <*>
               (optional $
                strOption (help "Resolved state"
                          <> long "resolved-state"   <> metavar "STATE")) <*>
               (optional $
                strOption (help "Tasks output"
                          <> long "output-tasks"     <> metavar "FILE"))  <*>
               (optional $
                strOption (help "Workitems output"
                          <> long "output-workitems" <> metavar "FILE")))
              (fullDesc <> progDesc "Generate a report"))))))
          (  fullDesc
          <> progDesc "Correlate a local git repository and Youtrack project with Branch issue field."
          <> header   "corr 0.0.7" ))
  when param_verbose $ printf "args: %s\n" $ show params
  when (any isNothing config) $ do
         when param_verbose $ printf "-- saving configuration in %s\n" config_filename
         save_config config_filename $ Config (Just param_server) (Just param_restprefix) (Just param_server_cacert) (Just param_project_id) (Just param_login)
  writeIORef params'ior params
  pure params


instance Functor ((,,) a b) where
    fmap f (x, y, z) = (x, y, f z)


-- * Get raw data from youtrack and git
yt_branchy_issues ∷ Params → IO (Maybe SomeException, (YT, Project, HM.HashMap RefName Issue))
yt_branchy_issues params@Params{..} = do
  iss_orelse ← case param_no_network of
                 True  → pure ∘ Left ∘ SomeException $ ErrorCall "Network access was disabled through the --no-network switch."
                 False → either_exception_or $ do
                                       (yt, projects) ← yt_connect params
                                       let project    = lookup_project projects $ PAlias param_project_id
                                       iss            ← yt_issues yt projects
                                                        (Filter $ printf "project: %s and br: -{No branch}" $ fromPAlias $ project_alias project)
                                                        aLL_ISSUES_PLEASe
                                       pure (yt, project, iss)
  let issue_branch i@Issue{issue_fields} =
          case fromMaybe (error $ printf "invariant failed: issue %s missing field 'Branch': %s" (show i) (show issue_fields)) $
               HM.lookup "Branch" issue_fields of
            AE.Array v
                | [AE.String t] ← V.toList v → T.unpack t
            mf → error $ "malformed issue field 'Branch', not a singular text vector: " <> show mf
  ret@(yt_failure, (_, _, iss)) ←
      fmap (fmap $ HM.fromList ∘ fmap (RefName ∘ issue_branch &&& id))
      <$> case iss_orelse of
            Right pr_iss → pure (Nothing, pr_iss)
            Left ex      → (Just ex,) ∘ (error "YT Project not accessible due to service access failure."
                                        ,error "YT Project not accessible due to service access failure."
                                        ,)
                           <$> sample' (arbitrary ∷ Gen Issue)
  when param_verbose $ do
    case yt_failure of
      Just ex → printf "-- YT access error:\n%s\n" (show ex)
      Nothing → pure ()
    printf ("-- all issues: %d %s\n") (length iss) (show $ fmap (fromIId ∘ issue_id) iss)

  pure ret

git_branches ∷ Params → IO (HM.HashMap RefName Branch)
git_branches Params{..} = do
  origin_branches ← gitrepo_branches param_repo
                    (\case
                      RemoteBranch (RefName name) (RemoteName "origin") → name ≢ "HEAD"
                      _ → False)

  when param_verbose $ do
    printf ("-- origin branches:   %d %s\n") (length origin_branches) (show $ HM.keys origin_branches)
  pure origin_branches


-- * Correlate: [Work] ← ([Branch], [Issue])

build_correlation_context ∷ Params → IO CorrCtx
build_correlation_context params@Params{..} = do
  -- * Obtain raw data
  origin_branches                             ← git_branches      params

  printf "-- Accesing YouTrack server, press C-c (or pass --no-network) to skip..\n"
  (yt_failure, (yt, project, branchy_issues)) ← yt_branchy_issues params

  -- * Correlate
  let --project        = lookup_project projects $ PAlias param_project_id
      all_work       = correlate_work project origin_branches branchy_issues
      work_branched  = HM.filter ((flip HM.member) origin_branches ∘ work_refname) all_work
      work_issued    = HM.filter ((flip HM.member) branchy_issues  ∘ work_refname) all_work

  pure CorrCtx { yt         = yt
               , yt_failure = yt_failure
               , project    = project
               , all_work   = all_work
               , wsets      = HM.fromList $
                              map (id *** map snd ∘ sortBy (compare `on` fst) ∘ HM.toList)
                              [ (GIT,     work_branched)
                              , (Work.YT, work_issued)
                              , (YTMGIT,  work_issued   `HM.difference` work_branched)
                              , (GITMYT,  work_branched `HM.difference` work_issued)
                              , (GITAYT,  work_branched `HM.union`      work_issued)] }


-- :main --server gra-tracker.ptsecurity.ru --cacert /home/deepfire/.cert/PTRootCA.crt --project GRA --login skosyrev branches git-yt
main ∷ IO ()
main = detailedFail $ do
  params@Params{..} ← get_params

  ctx@CorrCtx{..}  ← build_correlation_context params

  case param_cmd of
    Nothing  → printf "corr: no command specified.\n"
    Just cmd → action_handler ctx cmd

data  CorrCtx
    = CorrCtx { yt         ∷ YT
              , yt_failure ∷ Maybe SomeException
              , project    ∷ Project
              , all_work   ∷ HM.HashMap RefName Work
              , wsets      ∷ Worksets }

action_handler ∷ CorrCtx → Command → IO ()
action_handler CorrCtx{..} (CmdExec iiid cmd) = do
  let iid              = IId iiid
      pal              = project_alias project
      work_by_iid ∷ IId → Maybe Work
      work_by_iid iid' =
          fmap snd ∘ find ((identify_issue (project_alias project) iid') ∘ work_issue ∘ snd) $ HM.toList all_work
  work ← case work_by_iid iid of
           Just work → pure $ work
           Nothing → do
             i ← (fromMaybe $ error $ printf "While mutating issue %s: couldn't find it." $ fromIId iid)
                 <$> (project_issue project $ IId iiid)
             pure $ lift_work project Nothing (Just i) Nothing
             -- $ error $ P.printf "Couldn't find issue with id %s.\nKnown issues: %s" (show iid) (show ∘ sort ∘ fmap (fromIId ∘ issue_id ∘ work_issue ∘ snd) $ HM.toList all_work)
  if | isPrefixOf "description: " cmd
         → issue_update_description (work_issue work) ∘ Description $ drop 13 cmd -- XXX: work isn't updated!
     | otherwise
         → (ytRequestRaw yt $ RIssueExecute pal iid (YTCmd cmd) True) >> pure ()
  pure ()
action_handler CorrCtx{..} CmdUI = do
  let g = molecule "Available work" (VT.KBackTab, VT.KChar '\t') False
          [ (APt WTSet,   AtmF (derive_atom def                      ∷ BSSwitch))
          , (APt WTExpr,  AtmF (derive_atom (Name "<inputbar>")      ∷ FEditor))
          , (APt WTTable, AtmF (derive_atom (def, wsets, const True) ∷ WTable)) ]
  _ ← UI.run $
      case yt_failure of
        Nothing → [g]
        -- XXX: message story is broken
        Just e  → (:[]) ∘ molecule_set_popup "Error" g -- XXX: how to automatically reflow text in brick?
                  $  [ " "
                     , "  Continuable error while querying YouTrack server for issues:"
                     , " " ]
                  <> (concat ∘ map (chunksOf 80) ∘ lines $ show e)
                  <> [ " "
                     , "  Esc to continue with fake YT issues.."
                     , " " ]
  pure ()
action_handler CorrCtx{..} CmdReport{..} = do
  now ← getCurrentTime
  let (year, month, _) = toGregorian $ utctDay now
      period           = fromMaybe (printf "%d-%02d" year (month - 1)) report_period
      Project{..}      = project

  printf "--- total %d users:\n" $ length project_members
  forM_ project_members $ \Member {..} → do
         printf "%12s: %s\n" (fromMLogin member_login) (printFullNameEastern $ fromMFullName member_fullname)

  putStrLn "-- 2: querying issue list.."
  -- (flip yt_req) (RIssue (Filter $ "resolved date: " <> "2016-02") 100 []) =<< yt_connect (Access { hostname ="gra-tracker.ptsecurity.ru", ssl_opts = SSLOptions "" "/home/deepfire/.cert/PTRootCA.crt", login = "skosyrev" }) Nothing
  let req         = RIssue (Filter $ "resolved date: " <> period) 100 $ map Y.Field
                    ["projectShortName", "numberInProject", "created", "updated", "description", "Estimation", "reporterName", "resolved", "State", "summary", "Type", "Priority"]

  putStrLn "-- 3: interpreting issue list.."
  projects ← readIORef projects'ior
  iss ← ytRequest yt projects req
  printf "--- got %d issues\n" $ length iss

  putStrLn "-- 4: querying workitems.."
  wilists ← forM iss $ \i@(Issue{..}) → do
              wsrs ← ytRequest yt project $ RIssueTTWItem project_alias issue_id
              pure (i, wsrs)

  putStrLn "-- 5: building report.."
  let report = [ issue_report project_areapath project_product project i wis
               | (i, wis) ← wilists ]
      report_issues    = map fst report
      report_workitems = concat $ map snd report

  putStrLn "-- 6: CSV.."
  let handle_output Nothing      bytes = putStrLn ∘ BL.toString $ bytes
      handle_output (Just fname) bytes = BL.writeFile fname bytes

  handle_output tasks_output     $ CSV.encodeDefaultOrderedByName report_issues
  handle_output workitems_output $ CSV.encodeDefaultOrderedByName report_workitems
