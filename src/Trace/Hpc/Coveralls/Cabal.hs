{-# LANGUAGE CPP #-}

-- |
-- Module:      Trace.Hpc.Coveralls.Cabal
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for reading cabal package name and version.

module Trace.Hpc.Coveralls.Cabal (getPackageIdFromDir, getPackageFromDir, getPackageId) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.List (intercalate, isSuffixOf)
import Data.Semigroup ((<>))
import Distribution.Package (unPackageName, pkgName, pkgVersion)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Version
import System.Directory
import Trace.Hpc.Coveralls.Types (Package(Package), PackageIdentifier(PackageIdentifier))

getCabalFile :: FilePath -> IO (Maybe FilePath)
getCabalFile dir = do
    cabalFilesInDir <- filter isCabal <$> getDirectoryContents dir
    cabalFiles <- filterM doesFileExist (mkFullPath <$> cabalFilesInDir)
    case cabalFiles of
        [file] -> do
          putStrLn $ "getCabalFile: Found cabal file: " <> show file
          return $ Just file
        _ -> do
          putStrLn $ "getCabalFile: Didn't find cabal file in: " <> show dir
          return Nothing
    where
      isCabal filename = ".cabal" `isSuffixOf` filename && length filename > 6
      mkFullPath = ((dir <> "/") <>)

getPackageNameVersion :: FilePath -> IO (Maybe String)
getPackageNameVersion file = do
    orig <- readFile file
    case parsePackageDescription orig of
        ParseFailed _ -> return Nothing
        ParseOk _warnings gpd -> return $ Just $ name ++ "-" ++ version
            where pkg = package . packageDescription $ gpd
                  name = unPackageName $ pkgName pkg
                  version = showVersion (pkgVersion pkg)
                  showVersion = intercalate "." . map show . versionNumbers

dirPkgNameVer :: FilePath -> IO (Maybe String)
dirPkgNameVer = runMaybeT . pkgNameVersion
    where pkgNameVersion = MaybeT . getPackageNameVersion <=< MaybeT . getCabalFile

getPackageIdFromDir :: FilePath -> IO (Maybe PackageIdentifier)
getPackageIdFromDir = runMaybeT . pkgId
  where pkgId = MaybeT . getPackageId <=< MaybeT . getCabalFile

getPackageId :: FilePath -> IO (Maybe PackageIdentifier)
getPackageId file = do
  orig <- readFile file
  case parsePackageDescription orig of
    ParseFailed _ -> return Nothing
    ParseOk _warnings gpd -> return . Just $ PackageIdentifier name version
      where pkg = package . packageDescription $ gpd
            name = unPackageName $ pkgName pkg
            version = showVersion (pkgVersion pkg)
            showVersion = intercalate "." . map show . versionNumbers

getPackageFromDir :: FilePath -> IO (Maybe Package)
getPackageFromDir dir = do
  putStrLn $ "getPackageFromDir: dir:" <> show dir
  mPkgId <- getPackageIdFromDir dir
  putStrLn $ "getPackageFromDir: mPkgId:" <> show mPkgId
  pure $ Package dir <$> mPkgId

#if !(MIN_VERSION_Cabal(1,22,0))
unPackageName :: PackageName -> String
unPackageName (PackageName name) = name
#endif

#if !(MIN_VERSION_Cabal(2,0,0))
versionNumbers :: Version -> [Int]
versionNumbers = versionBranch
#endif
