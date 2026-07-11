{-# LANGUAGE NamedFieldPuns #-}

module Distribution.Server.Features.HaskellPlatform.Acid
  ( platformStateComponent
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.BackupRestore

import qualified Distribution.Server.Features.HaskellPlatform.State as Acid

platformStateComponent :: FilePath -> IO (StateComponent AcidState Acid.PlatformPackages)
platformStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Acid.PlatformPackages") Acid.initialPlatformPackages
  return StateComponent {
      stateDesc    = "Platform packages"
    , stateHandle  = st
    , getState     = query st Acid.GetPlatformPackages
    , putState     = update st . Acid.ReplacePlatformPackages
    , resetState   = platformStateComponent
    -- TODO: backup
    -- For now backup is just empty, as this package is basically featureless
    -- It defines state, but there is no way at all to modify this state
    , backupState  = \_ _ -> []
    , restoreState = RestoreBackup {
                         restoreEntry    = error "Unexpected backup entry for platform"
                       , restoreFinalize = return Acid.initialPlatformPackages
                       }
    }
