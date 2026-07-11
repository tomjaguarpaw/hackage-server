module Distribution.Server.Framework.Votes.Acid
  ( votesStateComponent
  ) where

import qualified Distribution.Server.Features.Votes.State as Acid

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

import qualified Data.Map as Map

-- | Define the backing store (i.e. database component)
votesStateComponent :: FilePath -> IO (StateComponent AcidState Acid.VotesState)
votesStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Votes") Acid.initialVotesState
  return StateComponent {
      stateDesc    = "Backing store for Map PackageName -> Users who voted for it"
    , stateHandle  = st
    , getState     = query st Acid.GetVotesState
    , putState     = update st . Acid.ReplaceVotesState
    , resetState   = votesStateComponent
    , backupState  = \_ _ -> []
    , restoreState = RestoreBackup {
                         restoreEntry    = error "Unexpected backup entry"
                       , restoreFinalize = return $ Acid.VotesState Map.empty
                       }
   }
