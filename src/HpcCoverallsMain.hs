module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.List
import           Data.Maybe hiding (listToMaybe)
import           Data.Semigroup ((<>))
import           HpcCoverallsCmdLine
import           System.Console.CmdArgs
import           System.Environment (getEnv, getEnvironment)
import           System.Exit (exitFailure)
import           Trace.Hpc.Coveralls
import           Trace.Hpc.Coveralls.Cabal
import           Trace.Hpc.Coveralls.Config (Config(Config, cabalFile, serviceName, testSuites, excludedDirs))
import           Trace.Hpc.Coveralls.Curl
import           Trace.Hpc.Coveralls.GitInfo (getGitInfo)
import           Trace.Hpc.Coveralls.Util
import           Trace.Hpc.Coveralls.Paths (firstExistingDirectory, hpcDirs, dumpDirectory, distDir, )

urlApiV1 :: String
urlApiV1 = "https://coveralls.io/api/v1/jobs"

getServiceAndJobID :: IO (String, String)
getServiceAndJobID = do
    env <- getEnvironment
    case snd <$> find (isJust . flip lookup env . fst) ciEnvVars of
        Just (ciName, jobIdVarName) -> do
            jobId <- getEnv jobIdVarName
            return (ciName, jobId)
        _ -> error "Unsupported CI service."
    where ciEnvVars = [
           ("TRAVIS",      ("travis-ci", "TRAVIS_JOB_ID")),
           ("CIRCLECI",    ("circleci",  "CIRCLE_BUILD_NUM")),
           ("SEMAPHORE",   ("semaphore", "REVISION")),
           ("JENKINS_URL", ("jenkins",   "BUILD_ID")),
           ("CI_NAME",     ("codeship",  "CI_BUILD_NUMBER"))]

writeJson :: String -> Value -> IO ()
writeJson filePath = BSL.writeFile filePath . encode

getConfig :: HpcCoverallsArgs -> Maybe Config
getConfig hca = Config
    (optExcludeDirs hca)
    (optCoverageMode hca)
    (optCabalFile hca)
    (optServiceName hca)
    (optRepoToken hca)
    <$> listToMaybe (argTestSuites hca)

main :: IO ()
main = do
  hca <- cmdArgs hpcCoverallsArgs
  case getConfig hca of
    Nothing -> putStrLn "Please specify a target test suite name"
    Just config -> do
      -- Determine hpcDir to use
      hpcDir <- case optHpcDirectory hca of
        -- If explicit hpcDir provided, use that
        (Just hpcDir) -> pure hpcDir
        -- Else try to discover hpcDir
        Nothing -> do
          mHpcDir <- firstExistingDirectory hpcDirs
          case mHpcDir of
            Nothing -> putStrLn "Couldn't find the hpc data directory" >> dumpDirectory distDir >> ioFailure
            Just hpcDir -> pure hpcDir

      -- Determine srcDir to use
      let srcDir = "."

      -- Collect and filter the coverage data
      let testSuiteNames = testSuites config
          excludedDirPatterns = excludedDirs config
          sourceDirFilter = not . matchAny excludedDirPatterns

      mPkgNameVer <- case cabalFile config of
        Just cabalFilePath -> getPackageNameVersion cabalFilePath
        Nothing -> currDirPkgNameVer
      coverageData <- getCoverageData mPkgNameVer hpcDir srcDir testSuiteNames
      let filteredCoverageData = filterCoverageData sourceDirFilter coverageData

      putStrLn (show filteredCoverageData)
      -- Convert data to coveralls format
      -- toCoverallsJson serviceName jobId repoTokenM gitInfo converter filteredCoverageData

-- main :: IO ()
-- main = do
--     hca <- cmdArgs hpcCoverallsArgs
--     case getConfig hca of
--         Nothing -> putStrLn "Please specify a target test suite name"
--         Just config -> do
--             (defaultServiceName, jobId) <- getServiceAndJobID
--             let sn = fromMaybe defaultServiceName (serviceName config)
--             gitInfo <- getGitInfo
--             mPkgNameVer <- case cabalFile config of
--                 Just cabalFilePath -> getPackageNameVersion cabalFilePath
--                 Nothing -> currDirPkgNameVer
--             gitInfo <- getGitInfo
--             coverallsJson <- generateCoverallsFromTix sn jobId gitInfo config mPkgNameVer
--             when (optDisplayReport hca) $ BSL.putStrLn $ encode coverallsJson
--             let filePath = sn ++ "-" ++ jobId ++ ".json"
--             writeJson filePath coverallsJson
--             unless (optDontSend hca) $ do
--                 response <- postJson filePath urlApiV1 (optCurlVerbose hca)
--                 case response of
--                     PostSuccess url -> do
--                         putStrLn ("URL: " ++ url)
--                         -- wait 10 seconds until the page is available
--                         threadDelay (10 * 1000 * 1000)
--                         coverageResult <- readCoverageResult url (optCurlVerbose hca)
--                         case coverageResult of
--                             Just totalCoverage -> putStrLn ("Coverage: " ++ totalCoverage)
--                             Nothing -> putStrLn "Failed to read total coverage"
--                     PostFailure msg -> do
--                         putStrLn ("Error: " ++ msg)
--                         putStrLn ("You can get support at " ++ gitterUrl)
--                         exitFailure
--                         where gitterUrl = "https://gitter.im/guillaume-nargeot/hpc-coveralls"
