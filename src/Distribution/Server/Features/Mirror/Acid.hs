module Distribution.Server.Features.Mirror.Acid
  ( mirrorersStateComponent
  ) where

import Distribution.Server.Prelude

import Distribution.Server.Framework
import Distribution.Server.Users.State

mirrorersStateComponent :: FilePath -> IO (StateComponent AcidState MirrorClients)
mirrorersStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "MirrorClients") initialMirrorClients
  return StateComponent {
      stateDesc    = "Mirror clients"
    , stateHandle  = st
    , getState     = query st GetMirrorClients
    , putState     = update st . ReplaceMirrorClients . mirrorClients
    , backupState  = \_ (MirrorClients clients) -> [csvToBackup ["clients.csv"] $ groupToCSV clients]
    , restoreState = MirrorClients <$> groupBackup ["clients.csv"]
    , resetState   = mirrorersStateComponent
    }
