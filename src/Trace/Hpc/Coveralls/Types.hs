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
import Network.Curl
import System.Console.CmdArgs.Default
import Trace.Hpc.Mix
import Data.Semigroup ((<>))

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


data PackageIdentifier
  = PackageIdentifier { pkgIdName    :: String
                      , pkgIdVersion :: String
                      }
  deriving (Eq, Show, Data)

asNameVer :: PackageIdentifier -> String
asNameVer (PackageIdentifier name ver) = name <> "-" <> ver
  
data Package
  = Package { pkgRootDir :: FilePath
            , pkgId      :: PackageIdentifier
            }
  deriving (Eq, Show, Data)
