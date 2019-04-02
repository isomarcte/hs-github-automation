module GitHub.Project.Automation
  ( orgTeamIssues
  , teamIssues
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad (Monad(..), foldM, join)
import Data.Either (Either)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Maybe (Maybe)
import Data.Traversable (traverse)
import GitHub.Project.Util (step, vectorToSeq)
import System.IO (IO)

import qualified Data.Map.Strict as DMS
import qualified Data.Sequence as DS
import qualified GitHub.Auth as GA
import qualified GitHub.Data.Definitions as GDD
import qualified GitHub.Data.Id as GDId
import qualified GitHub.Data.Issues as GDI
import qualified GitHub.Data.Name as GDN
import qualified GitHub.Data.Options as GDO
import qualified GitHub.Data.Repos as GDR
import qualified GitHub.Data.Teams as GDT
import qualified GitHub.Endpoints.Issues as GEI
import qualified GitHub.Endpoints.Organizations.Teams as GEOT
import qualified GitHub.Internal.Prelude as GIP

type RepoIssueMap = DMS.Map GDR.Repo (DS.Seq GDI.Issue)

type TeamRepoIssueMap = DMS.Map GDT.SimpleTeam RepoIssueMap

type IOE a = IO (Either GDD.Error a)

orgTeamIssues ::
     Maybe GA.Auth
  -> GDO.IssueRepoMod
  -> GDN.Name GDD.Organization
  -> IOE TeamRepoIssueMap
orgTeamIssues auth irm org = do
  evt <- GEOT.teamsOf' auth org
  ee <- traverse simpleTeamVectorToRepoIssueMap evt
  return $ join ee
  where
    simpleTeamToRepoIssueMap :: GDT.SimpleTeam -> IOE RepoIssueMap
    simpleTeamToRepoIssueMap st =
      let teamId = GDT.simpleTeamId st
       in teamIssues auth irm teamId
    simpleTeamVectorToRepoIssueMap ::
         GIP.Vector GDT.SimpleTeam -> IOE TeamRepoIssueMap
    simpleTeamVectorToRepoIssueMap =
      foldM (step simpleTeamToRepoIssueMap) (pure DMS.empty)

teamIssues ::
     Maybe GA.Auth -> GDO.IssueRepoMod -> GDId.Id GDT.Team -> IOE RepoIssueMap
teamIssues auth irm team = do
  etr <- GEOT.listTeamRepos' auth team
  ee <- traverse repoVectorToIssueMap etr
  return $ join ee
  where
    repoToIssueSeq :: GDR.Repo -> IOE (DS.Seq GDI.Issue)
    repoToIssueSeq r =
      let owner = GDD.simpleOwnerLogin . GDR.repoOwner $ r
          repoName = GDR.repoName r
       in repoIssues auth irm owner repoName
    repoVectorToIssueMap :: GIP.Vector GDR.Repo -> IOE RepoIssueMap
    repoVectorToIssueMap = foldM (step repoToIssueSeq) (pure DMS.empty)

repoIssues ::
     Maybe GA.Auth
  -> GDO.IssueRepoMod
  -> GDN.Name GDD.Owner
  -> GDN.Name GDR.Repo
  -> IOE (DS.Seq GDI.Issue)
repoIssues auth irm owner repo =
  fmap (fmap vectorToSeq) (GEI.issuesForRepo' auth owner repo irm)
