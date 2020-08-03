{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module:      Trace.Hpc.Coveralls
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for converting and sending hpc output to coveralls.io.

module Trace.Hpc.Coveralls ( generateCoverallsFromTix, ioFailure, getCoverageData, filterCoverageData, strictConverter, looseConverter, toCoverallsJson) where

import           Control.Applicative
import           Control.Monad (forM)
import           Data.Aeson
import           Data.Monoid ((<>), First(First, getFirst))
import           Data.Aeson.Types ()
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Digest.Pure.MD5
import           Data.Foldable (fold)
import           Data.Function
import           Data.List
import qualified Data.Map.Strict as M
import           System.Directory (doesDirectoryExist, doesFileExist)
import           System.Exit (exitFailure)
import           Trace.Hpc.Coveralls.Config
import           Trace.Hpc.Coveralls.GitInfo (GitInfo)
import           Trace.Hpc.Coveralls.Lix
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

readMix' :: Maybe String -> String -> String -> TixModule -> IO Mix
readMix' mPkgNameVer hpcDir name tix = do
  putStrLn ("hpcDir      : " <> hpcDir)
  putStrLn ("name        : " <> name)
  putStrLn ("tix         : " <> show tix)
  putStrLn ("mPkgNameVer : " <> show mPkgNameVer)
  putStrLn ("MixDirs     : " <> show dirs)
  readMix dirs (Right tix)
    where dirs = nub $ (\x -> getMixPath x hpcDir name tix) <$> [Nothing, mPkgNameVer]

-- | Create a list of coverage data from the tix input
readCoverageData :: Maybe String             -- ^ Package name-version
                 -> String                   -- ^ hpc data directory
                 -> [String]                 -- ^ excluded source folders
                 -> String                   -- ^ test suite name
                 -> IO TestSuiteCoverageData -- ^ coverage data list
readCoverageData mPkgNameVer hpcDir excludeDirPatterns testSuiteName = do
    let tixPath = getTixPath hpcDir testSuiteName
    mTix <- readTix tixPath
    case mTix of
        Nothing -> putStrLn ("Couldn't find the file " ++ tixPath) >> dumpDirectoryTree hpcDir >> ioFailure
        Just (Tix tixs) -> do
            mixs <- mapM (readMix' mPkgNameVer hpcDir testSuiteName) tixs
            let files = map filePath mixs
            sources <- mapM readFile files
            let coverageDataList = zip4 files sources mixs (map tixModuleTixs tixs)
            let filteredCoverageDataList = filter sourceDirFilter coverageDataList
            return $ M.fromList $ map toFirstAndRest filteredCoverageDataList
            where filePath (Mix fp _ _ _ _) = fp
                  sourceDirFilter = not . matchAny excludeDirPatterns . fst4

-- | Generate coveralls json formatted code coverage from hpc coverage data
generateCoverallsFromTix :: String       -- ^ CI name
                         -> String       -- ^ CI Job ID
                         -> GitInfo      -- ^ Git repo information
                         -> Config       -- ^ hpc-coveralls configuration
                         -> Maybe String -- ^ Package name-version
                         -> IO Value     -- ^ code coverage result in json format
generateCoverallsFromTix serviceName jobId gitInfo config mPkgNameVer = do
    mHpcDir <- firstExistingDirectory hpcDirs
    case mHpcDir of
        Nothing -> putStrLn "Couldn't find the hpc data directory" >> dumpDirectory distDir >> ioFailure
        Just hpcDir -> do
            testSuitesCoverages <- mapM (readCoverageData mPkgNameVer hpcDir excludedDirPatterns) testSuiteNames
            let coverageData = mergeCoverageData testSuitesCoverages
            return $ toCoverallsJson serviceName jobId repoTokenM gitInfo converter coverageData
            where excludedDirPatterns = excludedDirs config
                  testSuiteNames = testSuites config
                  repoTokenM = repoToken config
                  converter = case coverageMode config of
                      StrictlyFullLines -> strictConverter
                      AllowPartialLines -> looseConverter

getCoverageData
  :: Maybe String
  -- ^ Package name-version
  -> FilePath
  -- ^ HPC directory
  -> FilePath
  -- ^ Source directories
  -> [String]
  -- ^ Test suite names
  -> IO TestSuiteCoverageData
getCoverageData mPkgNameVer hpcDir srcDir testSuiteNames = do
  pathExists <- doesDirectoryExist hpcDir
  if pathExists == False
    then putStrLn "Couldn't find the hpc data directory" >> dumpDirectory hpcDir >> ioFailure
    else
      -- For each test suite
      foldFor testSuiteNames $ \testSuiteName -> do
      -- Read the tix file
        let tixPath = getTixPath hpcDir testSuiteName
        mTix <- readTix tixPath
        case mTix of
          Nothing -> putStrLn ("Couldn't find the file " ++ tixPath) >> dumpDirectoryTree hpcDir >> ioFailure
          Just tix@(Tix tixModules) -> do
      -- For each TixModule in the tix file
            foldFor tixModules $ \tixModule@(TixModule _ _ _ tixs) -> do
      -- Read the mix file
              mix@(Mix filePath _ _ _ _) <- readMix' mPkgNameVer hpcDir testSuiteName tixModule
      -- Read the source
              srcFile <- findSourceFile srcDir filePath
      -- Package source up with module tixs, indexed by the file path
              pure $ M.singleton filePath (source, mix, tixs)
      -- Sum all this up using the monoid instance for TestCoverageData,
      -- forming the total coverage data

findSourceFile
  :: FilePath
  -- ^ Source directories
  -> FilePath
  -- ^ File to look for
  -> IO FilePath
  -- ^ Found file
findSourceFile srcDir file = do
  subDirs <- do
    contents <- listDirectory srcDir
    foldFor contents $ \f -> do
      isDir <- doesDirectoryExist f
      if isDir
        then pure [f]
        else pure []
  let
    candidates :: [FilePath]
    candidates = srcDir : subDirs

  (mFullPath :: First FilePath) <- foldFor candidates $ \dir -> do
    let fullPath = dir <> "/" <> file
    fileExists <- doesFileExist fullPath
    if fileExists
      then pure (First (Just fullPath))
      else pure (mempty)

  let result = getFirst mFullPath

  case result of
    Nothing        -> putStrLn ("Couldn't find the source file " ++ filePath ++ " in directories: " <> show candidates <> ".") >> ioFailure
    (Just srcFile) -> pure srcFile

foldFor :: _
foldFor = flip foldMap
  

filterCoverageData :: (FilePath -> Bool)
                   -> TestSuiteCoverageData
                   -> TestSuiteCoverageData
filterCoverageData pred = M.filterWithKey (\k v -> pred k)

-- hpcDir, testSuiteNames, sourceDirFilter:

-- coverageData <- getCoverageData mPkgNameVer hpcDir testSuiteNames
-- let filteredCoverageData = filterCoverageData sourceDirFilter coverageData
-- return $ toCoverallsJson serviceName jobId repoTokenM gitInfo converter filteredCoverageData

-- semigroupOp is unionWith mergeCoverageData

ioFailure :: IO a
ioFailure = putStrLn ("You can get support at " ++ gitterUrl) >> exitFailure
    where gitterUrl = "https://gitter.im/guillaume-nargeot/hpc-coveralls" :: String
