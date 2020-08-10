{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Trace.Hpc.Coveralls
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for converting and sending hpc output to coveralls.io.

module Trace.Hpc.Coveralls ( generateCoverallsFromTix, findHpcDataDirs, findPackages, findTestSuiteNames, getCoverageData, strictConverter, looseConverter, toCoverallsJson) where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types ()
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Digest.Pure.MD5
import           Data.Function
import           Data.List
import           Data.Maybe (fromMaybe)
import           Data.Traversable (for)
import           Data.Semigroup (Semigroup((<>)))
import qualified Data.Map.Strict as M
import           System.Directory (doesDirectoryExist)
import           Data.Monoid (getFirst, First(First))
import           System.Exit (exitFailure)
import           System.Directory (doesFileExist, findFile)
import           Trace.Hpc.Coveralls.Config
import           Trace.Hpc.Coveralls.GitInfo (GitInfo)
import           Trace.Hpc.Coveralls.Lix
import           Trace.Hpc.Coveralls.Cabal
import           Trace.Hpc.Coveralls.Paths
import           Trace.Hpc.Coveralls.Types
import           Trace.Hpc.Coveralls.Util
import           Trace.Hpc.Mix
import           Trace.Hpc.Tix
import           Trace.Hpc.Util

type ModuleCoverageData = (
    String,    -- file source code
    Mix,       -- module index data
    [Integer]) -- tixs recorded by hpc

type TestSuiteCoverageData = M.Map FilePath ModuleCoverageData

-- single file coverage data in the format defined by coveralls.io
type SimpleCoverage = [CoverageValue]

-- Is there a way to restrict this to only Number and Null?
type CoverageValue = Value

type LixConverter = Lix -> SimpleCoverage

strictConverter :: LixConverter
strictConverter = map $ \lix -> case lix of
    Full       -> Number 1
    Partial    -> Number 0
    None       -> Number 0
    Irrelevant -> Null

looseConverter :: LixConverter
looseConverter = map $ \lix -> case lix of
    Full       -> Number 2
    Partial    -> Number 1
    None       -> Number 0
    Irrelevant -> Null

toSimpleCoverage :: LixConverter -> Int -> [CoverageEntry] -> SimpleCoverage
toSimpleCoverage convert lineCount = convert . toLix lineCount

getExprSource :: [String] -> MixEntry -> [String]
getExprSource source (hpcPos, _) = subSubSeq startCol endCol subLines
    where subLines = subSeq startLine endLine source
          startLine = startLine' - 1
          startCol = startCol' - 1
          (startLine', startCol', endLine, endCol) = fromHpcPos hpcPos

groupMixEntryTixs :: [(MixEntry, Integer, [String])] -> [CoverageEntry]
groupMixEntryTixs = map mergeOnLst3 . groupBy ((==) `on` fst . fst3)
    where mergeOnLst3 xxs@(x : _) = (map fst3 xxs, map snd3 xxs, trd3 x)
          mergeOnLst3 [] = error "mergeOnLst3 appliedTo empty list"

coverageToJson :: LixConverter -> FilePath -> ModuleCoverageData -> Value
coverageToJson converter filePath (source, mix, tixs) = object [
    "name" .= filePath,
    "source_digest" .= (show . md5 . LBS.pack) source,
    "coverage" .= coverage]
    where coverage = toSimpleCoverage converter lineCount mixEntriesTixs
          lineCount = length $ lines source
          mixEntriesTixs = groupMixEntryTixs mixEntryTixs
          mixEntryTixs = zip3 mixEntries tixs (map getExprSource' mixEntries)
          Mix _ _ _ _ mixEntries = mix
          getExprSource' = getExprSource $ lines source

toCoverallsJson :: String -> String -> Maybe String -> GitInfo -> LixConverter -> TestSuiteCoverageData -> Value
toCoverallsJson serviceName jobId repoTokenM gitInfo converter testSuiteCoverageData =
    object $ if serviceName == "travis-ci" then withRepoToken else withGitInfo
    where base = [
              "service_job_id" .= jobId,
              "service_name" .= serviceName,
              "source_files" .= toJsonCoverageList testSuiteCoverageData]
          toJsonCoverageList = map (uncurry $ coverageToJson converter) . M.toList
          withRepoToken = mcons (("repo_token" .=) <$> repoTokenM) base
          withGitInfo   = ("git" .= gitInfo) : withRepoToken

mergeModuleCoverageData :: ModuleCoverageData -> ModuleCoverageData -> ModuleCoverageData
mergeModuleCoverageData (source, mix, tixs1) (_, _, tixs2) =
    (source, mix, zipWith (+) tixs1 tixs2)

mergeCoverageData :: [TestSuiteCoverageData] -> TestSuiteCoverageData
mergeCoverageData = foldr1 (M.unionWith mergeModuleCoverageData)

readMix' :: [PackageIdentifier] -> [FilePath] -> String -> TixModule -> IO Mix
readMix' pkgIds hpcDirs name tix = readMix dirs (Right tix)
    where
      dirs        = nub $ (\p hpcDir -> getMixPath p hpcDir name tix) <$> (Nothing : (Just <$> pkgNameVers)) <*> hpcDirs
      pkgNameVers = asNameVer <$> pkgIds

-- | Create a list of coverage data from the tix input
readCoverageData :: [Package]                -- ^ Packages information
                 -> [FilePath]               -- ^ hpc data directories
                 -> [String]                 -- ^ excluded source folders
                 -> String                   -- ^ test suite name
                 -> IO TestSuiteCoverageData -- ^ coverage data list
readCoverageData pkgs hpcDirs excludeDirPatterns testSuiteName = do
    let tixFileLocations = possibleTixFileLocations hpcDirs testSuiteName
    mTixPath <- firstExistingFile tixFileLocations
    case mTixPath of
      Nothing      ->
        putStrLn ("Couldn't find any of the possible tix file locations: " ++ show tixFileLocations) >> ioFailure
      Just tixPath -> do
        mTix <- readTix tixPath
        case mTix of
            Nothing         ->
              putStrLn ("Couldn't read the file " ++ tixPath) >> ioFailure
            Just (Tix tixs) -> do
                mixs <- mapM (readMix' pkgIds hpcDirs testSuiteName) tixs
                let files = map filePath mixs
                projectFiles <- mapM (findProjectSourceFile pkgDirs) files
                sources      <- mapM readFile projectFiles
                let coverageDataList = zip4 projectFiles sources mixs (map tixModuleTixs tixs)
                let filteredCoverageDataList = filter sourceDirFilter coverageDataList
                return $ M.fromList $ map toFirstAndRest filteredCoverageDataList
                where filePath (Mix fp _ _ _ _) = fp
                      sourceDirFilter = not . matchAny excludeDirPatterns . fst4
                      pkgIds  = pkgId      <$> pkgs
                      pkgDirs = pkgRootDir <$> pkgs

                      removeLeading prefix fp = fromMaybe fp $ stripPrefix prefix fp

                      findProjectSourceFile :: [FilePath] -> FilePath -> IO FilePath
                      findProjectSourceFile pkgDirs fp = do
                        mFile <- findFile pkgDirs fp
                        case mFile of
                          Nothing ->
                            putStrLn ("Couldn't find the source file " ++ fp ++ " in directories: " <> show pkgDirs <> ".") >> ioFailure
                          (Just actualFilePath) ->
                            pure (removeLeading "./" $ -- To retain consistency with current reports
                                  actualFilePath)


-- | Generate coveralls json formatted code coverage from hpc coverage data
generateCoverallsFromTix :: String       -- ^ CI name
                         -> String       -- ^ CI Job ID
                         -> GitInfo      -- ^ Git repo information
                         -> Config       -- ^ hpc-coveralls configuration
                         -> [Package]    -- ^ Packages information
                         -> IO Value     -- ^ code coverage result in json format
generateCoverallsFromTix serviceName jobId gitInfo config pkgs = do
    hpcDirs <- findHpcDataDirs config
    testSuitesCoverages <- mapM (readCoverageData pkgs hpcDirs excludedDirPatterns) testSuiteNames
    let coverageData = mergeCoverageData testSuitesCoverages
    return $ toCoverallsJson serviceName jobId repoTokenM gitInfo converter coverageData
    where excludedDirPatterns = excludedDirs config
          testSuiteNames = testSuites config
          repoTokenM = repoToken config
          converter = case coverageMode config of
              StrictlyFullLines -> strictConverter
              AllowPartialLines -> looseConverter

getCoverageData
  :: [String]
  -- ^ Excluded source folders
  -> [FilePath]
  -- ^ HPC data directories
  -> [Package]
  -- ^ Packages
  -> [String]
  -- ^ Test suite names
  -> IO TestSuiteCoverageData
getCoverageData excludedDirPatterns hpcDirs pkgs testSuiteNames = do
  testSuitesCoverages <- mapM (readCoverageData pkgs hpcDirs excludedDirPatterns) testSuiteNames
  let coverageData = mergeCoverageData testSuitesCoverages
  pure coverageData


findHpcDataDirs :: Config -> IO [FilePath]
findHpcDataDirs config = do
  case hpcDirOverrides config of
    [] -> do
      mHpcDir <- firstExistingDirectory hpcDirs
      case mHpcDir of
        Nothing     -> putStrLn "Couldn't find the hpc data directory" >> dumpDirectory distDir >> ioFailure
        Just hpcDir -> pure [hpcDir]
    potentialHpcDirs -> do
      fmap concat . for potentialHpcDirs $ \potentialHpcDir -> do
        let hpcDir = potentialHpcDir <> "/"
        doesExist <- doesDirectoryExist hpcDir
        if doesExist == False
          then putStrLn ("The hpc data directory override provided does not exist: " <> hpcDir) >> ioFailure
          else pure [hpcDir]

findPackages :: Config -> IO [Package]
findPackages config =
  let
    currDir = "./"

    findPkgRequest :: FindPackageRequest
    findPkgRequest =
      case cabalFile config of
        Just cabalFilePath ->
          useExplicitCabalFiles [(currDir, Just cabalFilePath)]
        Nothing            ->
          let
            packageDirOverrides :: [FilePath]
            packageDirOverrides = packageDirs config
            packageDirs' :: [FilePath]
            packageDirs' =
              if length packageDirOverrides == 0
                then [currDir]
                else packageDirOverrides
          in
            searchTheseDirectories packageDirs'

    renderFindPackageRequestError :: FindPackageRequest -> String
    renderFindPackageRequestError request =
      let
        render :: (FilePath, Maybe FilePath) -> String
        render (_, Just cabalFilePath) = "\nAt location '" <> cabalFilePath <> "'"
        render (dir, Nothing)          = "\nIn directory '" <> dir <> "'"

        indent :: String -> String
        indent = (" " <>)
      in "Couldn't find cabal file..." <> foldMap (indent . render) request

  in do
    pkgs <- getPackages findPkgRequest
    case pkgs of
      [] -> putStrLn (renderFindPackageRequestError findPkgRequest) >> ioFailure
      ps -> pure ps

findTestSuiteNames :: Config -> [Package] -> IO [String]
findTestSuiteNames config pkgs = do
  case testSuites config of
    [] -> do
      let cabalFiles = pkgCabalFilePath <$> pkgs
      foldMap readTestSuiteNames cabalFiles
    testSuiteNames -> pure testSuiteNames

ioFailure :: IO a
ioFailure = putStrLn ("You can get support at " ++ gitterUrl) >> exitFailure
    where gitterUrl = "https://gitter.im/guillaume-nargeot/hpc-coveralls" :: String
