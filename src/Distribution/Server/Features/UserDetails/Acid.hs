{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell,
    NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.UserDetails.Acid where

import Distribution.Server.Features.UserDetails.Types
import Distribution.Server.Features.UserDetails.State
    (UserDetailsTable(..), emptyAccountDetails, emptyUserDetailsTable)
import Distribution.Server.Framework
import Distribution.Server.Framework.BackupDump
import Distribution.Server.Features.UserDetails.Backup

import Distribution.Server.Users.Types

import qualified Data.IntMap as IntMap
import Data.Text (Text)

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)


------------------------------
-- State queries and updates
--

getUserDetailsTable :: Query UserDetailsTable UserDetailsTable
getUserDetailsTable = ask

replaceUserDetailsTable :: UserDetailsTable -> Update UserDetailsTable ()
replaceUserDetailsTable = put

lookupUserDetails :: UserId -> Query UserDetailsTable (Maybe AccountDetails)
lookupUserDetails (UserId uid) = do
    UserDetailsTable tbl <- ask
    return $! IntMap.lookup uid tbl

setUserDetails :: UserId -> AccountDetails -> Update UserDetailsTable ()
setUserDetails (UserId uid) udetails = do
    UserDetailsTable tbl <- get
    put $! UserDetailsTable (IntMap.insert uid udetails tbl)

deleteUserDetails :: UserId -> Update UserDetailsTable Bool
deleteUserDetails (UserId uid) = do
    UserDetailsTable tbl <- get
    if IntMap.member uid tbl
      then do put $! UserDetailsTable (IntMap.delete uid tbl)
              return True
      else return False

setUserNameContact :: UserId -> Text -> Text -> Update UserDetailsTable ()
setUserNameContact (UserId uid) name email = do
    UserDetailsTable tbl <- get
    put $! UserDetailsTable (IntMap.alter upd uid tbl)
  where
    upd Nothing         = Just emptyAccountDetails { accountName = name, accountContactEmail = email }
    upd (Just udetails) = Just udetails            { accountName = name, accountContactEmail = email }

setUserAdminInfo :: UserId -> Maybe AccountKind -> Text -> Update UserDetailsTable ()
setUserAdminInfo (UserId uid) akind notes = do
    UserDetailsTable tbl <- get
    put $! UserDetailsTable (IntMap.alter upd uid tbl)
  where
    upd Nothing         = Just emptyAccountDetails { accountKind = akind, accountAdminNotes = notes }
    upd (Just udetails) = Just udetails            { accountKind = akind, accountAdminNotes = notes }

makeAcidic ''UserDetailsTable [
    --queries
    'getUserDetailsTable,
    'lookupUserDetails,
    --updates
    'replaceUserDetailsTable,
    'setUserDetails,
    'setUserNameContact,
    'setUserAdminInfo,
    'deleteUserDetails
  ]

-------------------------
-- State component
--

userDetailsStateComponent :: FilePath -> IO (StateComponent AcidState UserDetailsTable)
userDetailsStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "UserDetails") emptyUserDetailsTable
  return StateComponent {
      stateDesc    = "Extra details associated with user accounts, email addresses etc"
    , stateHandle  = st
    , getState     = query st GetUserDetailsTable
    , putState     = update st . ReplaceUserDetailsTable
    , backupState  = \backuptype users ->
        [csvToBackup ["users.csv"] (userDetailsToCSV backuptype users)]
    , restoreState = userDetailsBackup
    , resetState   = userDetailsStateComponent
    }
