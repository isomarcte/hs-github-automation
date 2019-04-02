module GitHub.Project.Automation
  ( unarchivedOrgIssues
  , teamIssues
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad (Monad(..), foldM, join)
import Data.Either (Either)
import Data.Foldable (foldl')
import Data.Function (($), (.), id)
import Data.Functor (fmap)
import Data.Maybe (Maybe)
import Data.Monoid (Monoid(..))
import Data.Traversable (traverse)
import GitHub.Project.Util (vectorToSeq)
import Prelude (undefined)
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
import qualified GitHub.Endpoints.Repos as GER
import qualified GitHub.Internal.Prelude as GIP

unarchivedOrgIssues ::
     Maybe GA.Auth
  -> GDN.Name GDD.Organization
  -> IO (Either GDD.Error (DMS.Map GDT.Team (DMS.Map GDR.Repo (DS.Seq GDI.Issue))))
unarchivedOrgIssues auth org = undefined
  -- GEOT.teamsOf' auth org

teamIssues ::
     Maybe GA.Auth
  -> GDO.IssueRepoMod
  -> GDId.Id GDT.Team
  -> IO (Either GDD.Error (DMS.Map GDR.Repo (DS.Seq GDI.Issue)))
teamIssues auth irm team = do
  etr <- GEOT.listTeamRepos' auth team
  ee <- traverse repoVectorToIssueMap etr
  return $ join ee
  where
    repoToIssueMap ::
         DMS.Map GDR.Repo (DS.Seq GDI.Issue)
      -> GDR.Repo
      -> IO (Either GDD.Error (DMS.Map GDR.Repo (DS.Seq GDI.Issue)))
    repoToIssueMap m r =
      let owner = GDD.simpleOwnerLogin . GDR.repoOwner $ r
          repoName = GDR.repoName r
       in do e <- repoIssues auth irm owner repoName
             return $ fmap (\v -> DMS.insert r v m) e
    repoToIssueMap' ::
         Either GDD.Error (DMS.Map GDR.Repo (DS.Seq GDI.Issue))
      -> GDR.Repo
      -> IO (Either GDD.Error (DMS.Map GDR.Repo (DS.Seq GDI.Issue)))
    repoToIssueMap' em r = do
      ee <- traverse (\m -> repoToIssueMap m r) em
      return $ join ee
    repoVectorToIssueMap ::
         GIP.Vector GDR.Repo
      -> IO (Either GDD.Error (DMS.Map GDR.Repo (DS.Seq GDI.Issue)))
    repoVectorToIssueMap v = foldM repoToIssueMap' (pure DMS.empty) v

repoIssues ::
     Maybe GA.Auth
  -> GDO.IssueRepoMod
  -> GDN.Name GDD.Owner
  -> GDN.Name GDR.Repo
  -> IO (Either GDD.Error (DS.Seq GDI.Issue))
repoIssues auth irm owner repo =
  fmap (fmap vectorToSeq) (GEI.issuesForRepo' auth owner repo irm)
