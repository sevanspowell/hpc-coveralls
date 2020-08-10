{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module:      Trace.Hpc.Coveralls.Types
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Types to represent hpc code coverage data.

module Trace.Hpc.Coveralls.Types where

import Data.Data
import Data.Semigroup ((<>))
import Network.Curl
import System.Console.CmdArgs.Default
import Trace.Hpc.Mix

type CoverageEntry = (
    [MixEntry], -- mix entries
    [Integer],  -- tix values
    [String])   -- entry source code

data Hit = Full
         | Partial
         | None
         | Irrelevant
    deriving (Eq, Show)

type Lix = [Hit]

instance Default CoverageMode where
    def = AllowPartialLines

data CoverageMode = StrictlyFullLines
                  | AllowPartialLines
    deriving (Data, Eq, Show, Typeable)

-- | Result to the POST request to coveralls.io
data PostResult =
    PostSuccess URLString -- ^ Coveralls job url
  | PostFailure String    -- ^ error message

-- | Name and version used to identify a package.
data PackageIdentifier
  = PackageIdentifier { pkgIdName    :: String
                      , pkgIdVersion :: String
                      }
  deriving (Eq, Show)

-- | Get package identifier formatted as: "$name-$ver".
asNameVer :: PackageIdentifier -> String
asNameVer (PackageIdentifier name ver) = name <> "-" <> ver
  
-- | Description of a package from the perspective of hpc-coveralls.
data Package
  = Package { pkgRootDir :: FilePath
            , pkgId      :: PackageIdentifier
            }
  deriving (Eq, Show)

type FindPackageRequest
  = [
      ( FilePath
      -- ^ Project root directory
      , Maybe FilePath
      -- ^ Optional explicit path to cabal file
      )
    ]

searchTheseDirectories :: [FilePath] -> FindPackageRequest
searchTheseDirectories = fmap (\f -> (f, Nothing))

useExplicitCabalFiles :: [(FilePath, Maybe FilePath)] -> FindPackageRequest
useExplicitCabalFiles = id
