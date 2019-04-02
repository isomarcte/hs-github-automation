module GitHub.Project.Automation
  ( teamIssues
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad (Monad(..), foldM, join)
import Data.Either (Either)
import Data.Function (($), (.))
import Data.Functor (fmap)
import Data.Maybe (Maybe)
import Data.Ord (Ord)
import Data.Traversable (traverse)
import GitHub.Project.Util (vectorToSeq)
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

-- type TeamRepoIssueMap
--    = DMS.Map GDT.SimpleTeam (DMS.Map GDR.Repo (DS.Seq GDI.Issue))
type RepoIssueMap = DMS.Map GDR.Repo (DS.Seq GDI.Issue)

step ::
     (Monad m, Monad g, Ord k)
  => (k -> g (m v))
  -> m (DMS.Map k v)
  -> k
  -> g (m (DMS.Map k v))
step f mm k = do
  mv <- f k
  return $ do
    m <- mm
    v <- mv
    return $ DMS.insert k v m

-- orgTeamIssues ::
--      Maybe GA.Auth
--   -> GDO.IssueRepoMod
--   -> GDN.Name GDD.Organization
--   -> IO (Either GDD.Error TeamRepoIssueMap)
-- orgTeamIssues auth irm org = undefined
--   -- evt <- GEOT.teamsOf' auth org
--   where
--     simpleTeamToRepoIssueMap ::
--       -> GDT.SimpleTeam
--       -> IO (Either GDD.Error TeamRepoIssueMap)
--     simpleTeamToRepoIssueMap m st =
--       let teamId = GDT.simpleTeamId st
--        in do erim <- teamIssues auth irm teamId
--              return $ fmap (\v -> DMS.insert st v m) erim
teamIssues ::
     Maybe GA.Auth
  -> GDO.IssueRepoMod
  -> GDId.Id GDT.Team
  -> IO (Either GDD.Error RepoIssueMap)
teamIssues auth irm team = do
  etr <- GEOT.listTeamRepos' auth team
  ee <- traverse repoVectorToIssueMap etr
  return $ join ee
  where
    repoToIssueSeq :: GDR.Repo -> IO (Either GDD.Error (DS.Seq GDI.Issue))
    repoToIssueSeq r =
      let owner = GDD.simpleOwnerLogin . GDR.repoOwner $ r
          repoName = GDR.repoName r
       in repoIssues auth irm owner repoName
    repoVectorToIssueMap ::
         GIP.Vector GDR.Repo -> IO (Either GDD.Error RepoIssueMap)
    repoVectorToIssueMap = foldM (step repoToIssueSeq) (pure DMS.empty)

repoIssues ::
     Maybe GA.Auth
  -> GDO.IssueRepoMod
  -> GDN.Name GDD.Owner
  -> GDN.Name GDR.Repo
  -> IO (Either GDD.Error (DS.Seq GDI.Issue))
repoIssues auth irm owner repo =
  fmap (fmap vectorToSeq) (GEI.issuesForRepo' auth owner repo irm)
