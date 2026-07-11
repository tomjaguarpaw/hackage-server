{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}
module Distribution.Server.Features.HaskellPlatform (
    PlatformFeature,
    PlatformResource(..),
    initPlatformFeature,
  ) where

import Distribution.Server.Framework

import Distribution.Server.Features.HaskellPlatform.Acid (platformStateComponent)
import qualified Distribution.Server.Features.HaskellPlatform.State as Acid

import Distribution.Package
import Distribution.Version
import Distribution.Text

import Data.Function
import qualified Data.Map as Map
import qualified Data.Set as Set


-- Note: this can be generalized into dividing Hackage up into however many
-- subsets of packages are desired. One could implement a Debian-esque system
-- with this sort of feature.
--

data PlatformFeature = PlatformFeature {
    platformFeatureInterface :: HackageFeature,

    platformResource :: PlatformResource,

    platformVersions      :: forall m. MonadIO m => PackageName -> m [Version],
    platformPackageLatest :: forall m. MonadIO m => m [(PackageName, Version)],
    setPlatform           :: forall m. MonadIO m => PackageName -> [Version] -> m (),
    removePlatform        :: forall m. MonadIO m => PackageName -> m ()
}

instance IsHackageFeature PlatformFeature where
    getFeatureInterface = platformFeatureInterface

data PlatformResource = PlatformResource {
    platformPackage :: Resource,
    platformPackages :: Resource,
    platformPackageUri :: String -> PackageName -> String,
    platformPackagesUri :: String -> String
}

initPlatformFeature :: ServerEnv -> IO (IO PlatformFeature)
initPlatformFeature ServerEnv{serverStateDir} = do
    platformState <- platformStateComponent serverStateDir

    return $ do
      let feature = platformFeature platformState
      return feature

platformFeature :: StateComponent AcidState Acid.PlatformPackages
                -> PlatformFeature
platformFeature platformState
  = PlatformFeature{..}
  where
    platformFeatureInterface = (emptyHackageFeature "platform") {
        featureDesc = "List packages which are part of the Haskell platform (this is work in progress)"
      , featureResources =
          map ($ platformResource) [
              platformPackage
            , platformPackages
            ]
      , featureState = [abstractAcidStateComponent platformState]
      }

    platformResource = fix $ \r -> PlatformResource
      { platformPackage = (resourceAt "/platform/package/:package.:format") {
            resourceGet    = []
          , resourceDelete = []
          , resourcePut    = []
          }
      , platformPackages = (resourceAt "/platform/.:format") {
            resourceGet  = []
          , resourcePost = []
          }
      , platformPackageUri = \format pkgid ->
          renderResource (platformPackage r) [display pkgid, format]
      , platformPackagesUri = \format ->
          renderResource (platformPackages r) [format]
       -- and maybe "/platform/haskell-platform.cabal"
      }

    ------------------------------------------
    -- functionality: showing status for a single package, and for all packages, adding a package, deleting a package
    platformVersions :: MonadIO m => PackageName -> m [Version]
    platformVersions pkgname = liftM Set.toList $ queryState platformState $ Acid.GetPlatformPackage pkgname

    platformPackageLatest :: MonadIO m => m [(PackageName, Version)]
    platformPackageLatest = liftM (Map.toList . Map.map Set.findMax . Acid.blessedPackages) $ queryState platformState Acid.GetPlatformPackages

    setPlatform :: MonadIO m => PackageName -> [Version] -> m ()
    setPlatform pkgname versions = updateState platformState $ Acid.SetPlatformPackage pkgname (Set.fromList versions)

    removePlatform :: MonadIO m => PackageName -> m ()
    removePlatform pkgname = updateState platformState $ Acid.SetPlatformPackage pkgname Set.empty

