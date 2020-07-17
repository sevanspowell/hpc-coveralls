{-# LANGUAGE CPP #-}

-- |
-- Module:      Trace.Hpc.Coveralls.Cabal
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for reading cabal package name and version.

module Trace.Hpc.Coveralls.Cabal (dirPkgNameVer, getPackageNameVersion) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.List (intercalate, isSuffixOf)
import Data.Semigroup ((<>))
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Version
import System.Directory

getCabalFile :: FilePath -> IO (Maybe FilePath)
getCabalFile dir = do
    dirContents <- getDirectoryContents dir
    let
      cabalFiles = filter isCabal dirContents
      -- If the directory isn't ".", we need to make sure that the
      -- directory is added to the cabal file path in order for
      -- "doesFileExist" to find it.
      cabalFilesFullPath = ((dir <> "/") <>) <$> cabalFiles

    realCabalFiles <- filterM doesFileExist cabalFilesFullPath

    case realCabalFiles of
        [file] -> return $ Just file
        _ -> return Nothing
    where isCabal filename = ".cabal" `isSuffixOf` filename && length filename > 6

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

#if !(MIN_VERSION_Cabal(1,22,0))
unPackageName :: PackageName -> String
unPackageName (PackageName name) = name
#endif

#if !(MIN_VERSION_Cabal(2,0,0))
versionNumbers :: Version -> [Int]
versionNumbers = versionBranch
#endif
