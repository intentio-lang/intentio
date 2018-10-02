module TestRunner
  ( runDriver
  )
where

import           Intentio.Prelude

import           Control.Concurrent.Async       ( mapConcurrently )
import           System.Exit                    ( exitSuccess
                                                , exitFailure
                                                )

import           TestRunner.Executor            ( TestResult
                                                , TestError
                                                , prettyTestError
                                                , runTestCase
                                                , testResultState
                                                , testResultCase
                                                )
import           TestRunner.Model               ( TestCase
                                                , testCases
                                                , testCaseName
                                                )
import           TestRunner.Opts                ( Opts
                                                , rootDir
                                                , readOpts
                                                )
import           TestRunner.Scanner             ( scanDirectory )

data Summary = Summary
  { _totalTestCount  :: Int
  , _failedTestCount :: Int
  , _failedTests     :: [(TestCase, TestError)]
  }
  deriving (Show)

makeLenses ''Summary

runDriver :: IO ()
runDriver = do
  opts  <- readOpts
  suite <- scanDirectory (opts ^. rootDir)

  putText $ "Running " <> show (suite ^. testCases & length) <> " tests..."
  putText ""

  results <- mapConcurrently (runAndLog opts) $ suite ^. testCases

  let summary = collectSummary results

  putText ""
  printSummary summary
  putText ""

  if summary ^. failedTestCount > 0 then exitFailure else exitSuccess

runAndLog :: Opts -> TestCase -> IO TestResult
runAndLog opts testCase = do
  result <- runTestCase opts testCase
  let name = result ^. testResultCase . testCaseName
  case result ^. testResultState of
    Right _ -> putText $ "[ OK ] " <> name
    Left  _ -> putText $ "[FAIL] " <> name
  return result

collectSummary :: [TestResult] -> Summary
collectSummary = foldr' f init
 where
  init = Summary {_totalTestCount = 0, _failedTestCount = 0, _failedTests = []}

  f result summary =
    let testCase = result ^. testResultCase
        summary' = summary & (totalTestCount +~ 1)
    in  case result ^. testResultState of
          Right _ -> summary'
          Left err ->
            summary'
              & (failedTestCount +~ 1)
              & (failedTests %~ ((testCase, err) :))

printSummary :: Summary -> IO ()
printSummary summary
  | summary ^. failedTestCount == 0
  = let tc = show (summary ^. totalTestCount)
    in  putText $ tc <> " tests passed."
  | otherwise
  = do
    let fc = show (summary ^. failedTestCount)
    let tc = show (summary ^. totalTestCount)
    putText $ fc <> "/" <> tc <> " tests failed:"
    putText ""
    forM_ (summary ^. failedTests) $ \(testCase, err) -> do
      let n = testCase ^. testCaseName
      let p = prettyTestError err
      putText $ "=== " <> n <> " ==="
      putText p
      putText ""
