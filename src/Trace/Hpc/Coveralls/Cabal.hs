{-# LANGUAGE CPP #-}

-- |
-- Module:      Trace.Hpc.Coveralls.Cabal
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for reading cabal package name and version.

module Trace.Hpc.Coveralls.Cabal (getPackageId, getPackageNameVersion, getPackageFromDir, getPackages) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.List (intercalate, isSuffixOf)
import Distribution.Package (unPackageName, pkgName, pkgVersion)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Version
import System.Directory
import Trace.Hpc.Coveralls.Types

getCabalFile :: FilePath -> IO (Maybe FilePath)
getCabalFile dir = do
    cabalFilesInDir <- filter isCabal <$> getDirectoryContents dir
    cabalFiles <- filterM doesFileExist (mkFullPath <$> cabalFilesInDir)
    case cabalFiles of
        [file] -> do
          return $ Just file
        _ -> do
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

currDirPkgNameVer :: IO (Maybe String)
currDirPkgNameVer = runMaybeT $ pkgNameVersion currentDir
    where pkgNameVersion = MaybeT . getPackageNameVersion <=< MaybeT . getCabalFile
          currentDir = "."

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
  mPkgId <- getPackageIdFromDir dir
  pure $ Package dir <$> mPkgId

-- | Get a list of packages.
--
-- This function works by finding cabal files and parsing them to
-- provide package descriptions. You can provide either a full cabal
-- file paths (for legacy reasons) or directories containing cabal
-- files. Both will be used to generate a list of packages. If you
-- provide none, the current directory will be searched.
getPackages
  :: FindPackageRequest
  -> IO [Package]
getPackages = foldr foldF (pure []) 
  where
    foldF :: (FilePath, Maybe FilePath) -> IO [Package] -> IO [Package]
    foldF x acc  = do
      mPkg <- iter x
      case mPkg of
        Nothing  -> acc
        Just pkg -> (pkg:) <$> acc

    iter :: (FilePath, Maybe FilePath) -> IO (Maybe Package)
    iter (rootDir, Just cabalFilePath) = do
      mPkgId <- getPackageId cabalFilePath
      pure $ Package rootDir <$> mPkgId
    iter (rootDir, Nothing) = getPackageFromDir rootDir

#if !(MIN_VERSION_Cabal(1,22,0))
unPackageName :: PackageName -> String
unPackageName (PackageName name) = name
#endif

#if !(MIN_VERSION_Cabal(2,0,0))
versionNumbers :: Version -> [Int]
versionNumbers = versionBranch
#endif
