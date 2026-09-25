{-# LANGUAGE RankNTypes #-}

module Distribution.Server.Features.AdminLog.Store
  ( Backend(..)
  , Store(..)
  ) where

import Distribution.Server.Features.AdminLog.Types (AdminLogEntry)
import Distribution.Server.Framework (AbstractStateComponent)

import Control.Monad.Trans (MonadIO)

data Backend = Backend {
    backendStore :: Store
  , backendState :: [AbstractStateComponent]
  }

data Store = Store {
    getAdminLog :: forall m. MonadIO m => m [AdminLogEntry]
  , addAdminLog :: forall m. MonadIO m => AdminLogEntry -> m ()
  }
