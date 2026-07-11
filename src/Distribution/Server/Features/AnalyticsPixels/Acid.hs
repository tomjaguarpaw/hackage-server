module Distribution.Server.Features.AnalyticsPixels.Acid
  ( analyticsPixelsStateComponent
  ) where

import qualified Distribution.Server.Features.AnalyticsPixels.State as Acid
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

-- | Define the backing store (i.e. database component)
analyticsPixelsStateComponent :: FilePath -> IO (StateComponent AcidState Acid.AnalyticsPixelsState)
analyticsPixelsStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "AnalyticsPixels") Acid.initialAnalyticsPixelsState
  return StateComponent {
      stateDesc    = "Backing store for AnalyticsPixels feature"
    , stateHandle  = st
    , getState     = query st Acid.GetAnalyticsPixelsState
    , putState     = update st . Acid.ReplaceAnalyticsPixelsState
    , resetState   = analyticsPixelsStateComponent
    , backupState  = \_ _ -> []
    , restoreState = RestoreBackup {
                         restoreEntry    = error "Unexpected backup entry"
                       , restoreFinalize = return Acid.initialAnalyticsPixelsState
                       }
   }
