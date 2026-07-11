# Acid-State Components

Build note: to build the project, install `libgd-dev` with:

```sh
sudo apt-get install libgd-dev
```

For the refactors on this branch, each component follows the same pattern:

- Move the `<component>StateComponent` function into the component's `Acid` module.
- Introduce a component-local abstraction layer with `Backend` and `Store`.
- Instantiate the `Backend` from the existing acid-state implementation in a function called `acidStore`.

## Follow-up observations

- `src/Distribution/Server/Pages/Index.hs` builds the old category package
  index from `PackageIndex.allPackagesByName` using
  `maximumBy (comparing packageVersion)`. This is odd because other code treats
  the last package in each `PackageIndex` bucket as the latest version. Before
  replacing this consumer with `queryLatestPackages`, check whether it reflects
  uncertainty about bucket ordering, or whether the page needs subtly different
  semantics.

- `src/Distribution/Server/Features/Sitemap.hs` uses
  `PackageIndex.allPackagesByName` and then `head` for unversioned package and
  documentation sitemap entries, despite comments saying those URLs redirect to
  the latest version. Since package buckets appear to be sorted from oldest to
  newest, check whether these `head` uses are pre-existing bugs before
  abstracting this consumer.

- `AdminLog`
- `AnalyticsPixelsState`
- `BuildReports`
- `CandidatePackages`
- `Distros`
- `Documentation`
- `HackageAdmins`
- `HackageTrustees`
- `HackageUploaders`
- `InMemStats`
- `MirrorClients`
- `NotifyData`
- `PackageMaintainers`
- `PackageTags`
- `PackagesState`
- `PlatformPackages`
- `PreferredVersions`
- `SecurityState`
- `SignupResetTable`
- `TagAlias`
- `TarIndexCache`
- `UserDetailsTable`
- `Users.Users`
- `VouchData`
- `VotesState`
