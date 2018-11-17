{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module TestRunner.Executor
  ( TestResult(..)
  , TestError
  , prettyTestError
  , testResultCase
  , testResultState
  , runTestCase
  )
where

import           Intentio.Prelude

import           Control.Monad.Except           ( liftEither )
import           Data.Algorithm.Diff            ( Diff(..)
                                                , getGroupedDiff
                                                )
import           Data.Algorithm.DiffOutput      ( ppDiff )
import qualified Data.String                   as Str
import qualified Data.Text                     as T
import           System.Directory               ( getTemporaryDirectory )
import           System.FilePath                ( (</>)
                                                , takeDirectory
                                                )
import           System.IO.Temp                 ( createTempDirectory
                                                , withSystemTempDirectory
                                                )
import qualified System.Process.Typed          as P

import           TestRunner.Loader              ( LoaderError
                                                , prettyLoaderError
                                                , loadTestSpec
                                                )
import           TestRunner.Model               ( TestCase
                                                , TestSpec
                                                , TestCommand(..)
                                                , IOSpec(..)
                                                , argv
                                                , commands
                                                , compileCommandArgs
                                                , runCommandStderr
                                                , runCommandStdin
                                                , runCommandStdout
                                                , testCasePath
                                                )
import           TestRunner.Opts                ( Opts
                                                , compilerPath
                                                , runEntrypointPath
                                                , noCleanup
                                                )

data TestError
  = LoaderError LoaderError
  | CompilationFailure Int ByteString ByteString
  | RunCalledWithoutCompile
  | RunExitFailure Int ByteString ByteString
  | StdoutMismatch [Diff [String]]
  | StderrMismatch [Diff [String]]
  deriving (Show)

data TestResult = TestResult
  { _testResultCase :: TestCase
  , _testResultState :: Either TestError ()
  }
  deriving (Show)

data CommandState = CommandState
  { _stateOpts      :: Opts
  , _stateTmpDir    :: FilePath
  , _stateTestCase  :: TestCase
  , _compiledBinary :: Maybe FilePath
  }

type RunSpecMT = ExceptT TestError IO
type CommandMT = StateT CommandState RunSpecMT

makeLenses ''TestResult
makeLenses ''CommandState

outputBinaryName :: FilePath
outputBinaryName = "testbin.out"

testCaseCwd :: TestCase -> FilePath
testCaseCwd testCase = testCase ^. testCasePath & takeDirectory

prettyTestError :: TestError -> Text
prettyTestError (LoaderError e) = prettyLoaderError e
prettyTestError (CompilationFailure ec out err) =
  T.unlines $ [ecs, ""] <> outs <> errs err
 where
  ecs  = "Compilation failed with exit code: " <> show ec
  outs = T.lines $ toS out
  errs "" = []
  errs bs = ["", "--- Standard error ---"] <> T.lines (toS bs)
prettyTestError RunCalledWithoutCompile =
  "Bad test specification: RUN command called without preceding COMPILE command"
prettyTestError (RunExitFailure ec out err) =
  T.unlines $ ecs <> outs out <> errs err
 where
  ecs = ["Application failed with exit code: " <> show ec]

  outs "" = []
  outs bs = ["", "--- Standard output ---"] <> T.lines (toS bs)

  errs "" = []
  errs bs = ["", "--- Standard error ---"] <> T.lines (toS bs)
prettyTestError (StdoutMismatch d) =
  T.unlines ["Standard output mismatch:", toS $ ppDiff d]
prettyTestError (StderrMismatch d) =
  T.unlines ["Standard error mismatch:", toS $ ppDiff d]

runTestCase :: Opts -> TestCase -> IO TestResult
runTestCase opts testCase = do
  let _testResultCase = testCase
  _testResultState <-
    runExceptT $ loadTestSpec' testCase >>= runTestSpec opts testCase
  return TestResult { .. }

loadTestSpec' :: TestCase -> RunSpecMT TestSpec
loadTestSpec' testCase =
  testCase & loadTestSpec & liftIO <&> (_Left %~ LoaderError) >>= liftEither

runTestSpec :: Opts -> TestCase -> TestSpec -> RunSpecMT ()
runTestSpec opts testCase spec = void $ do
  if opts ^. noCleanup
    then do
      t1 <- lift getTemporaryDirectory
      t2 <- lift $ createTempDirectory t1 dirName
      f t2
    else withSystemTempDirectory dirName f
 where
  dirName = "intentio-test"

  f tmpdir = do
    execStateT (mapM_ runCommand (spec ^. commands))
               (emptyCommandState opts tmpdir testCase)

emptyCommandState :: Opts -> FilePath -> TestCase -> CommandState
emptyCommandState _stateOpts _stateTmpDir _stateTestCase =
  CommandState { _compiledBinary = Nothing, .. }

runCommand :: TestCommand -> CommandMT ()
runCommand (CompileCommand spec) = do
  myCompilerPath <- use (stateOpts . compilerPath)
  myTmpDir       <- use stateTmpDir
  myCwd          <- testCaseCwd <$> use stateTestCase
  let outputBinaryPath = myTmpDir </> outputBinaryName
  let myCompilerArgs =
        ["--workdir", myTmpDir, "-o", outputBinaryPath]
          <> (spec ^. compileCommandArgs . argv <&> toS)
  (exitCode, compilerStdout, compilerStderr) <-
    P.readProcess . P.setWorkingDir myCwd $ P.proc myCompilerPath myCompilerArgs
  case exitCode of
    ExitSuccess    -> compiledBinary .= Just outputBinaryPath
    ExitFailure ec -> throwError
      $ CompilationFailure ec (toS compilerStdout) (toS compilerStderr)

runCommand (RunCommand spec) = do
  myRunEntrypointPath <- use (stateOpts . runEntrypointPath)
  myCompiledBinary    <- use compiledBinary >>= \case
    Just p  -> return p
    Nothing -> throwError RunCalledWithoutCompile
  let myCwd    = takeDirectory myCompiledBinary
  let procPath = decorateProcPath myRunEntrypointPath (myCompiledBinary, [])
  let procConf =
        P.setStdin (buildStdinSpec $ spec ^. runCommandStdin)
          . P.setWorkingDir myCwd
          $ mkProc procPath
  (exitCode, actualStdout, actualStderr) <- P.readProcess procConf
  case exitCode of
    ExitFailure ec ->
      throwError $ RunExitFailure ec (toS actualStdout) (toS actualStderr)
    ExitSuccess -> do
      matchOutput (toS actualStdout) (spec ^. runCommandStdout) StdoutMismatch
      matchOutput (toS actualStderr) (spec ^. runCommandStderr) StderrMismatch

decorateProcPath
  :: Maybe FilePath -> (FilePath, [String]) -> (FilePath, [String])
decorateProcPath Nothing  p        = p
decorateProcPath (Just w) (pp, pa) = (w, pp : pa)

mkProc :: (FilePath, [String]) -> P.ProcessConfig () () ()
mkProc (pa, pp) = P.proc pa pp

buildStdinSpec :: Maybe IOSpec -> P.StreamSpec _ ()
buildStdinSpec Nothing                 = P.closed
buildStdinSpec (Just (RawIOSpec text)) = P.byteStringInput $ toS text
buildStdinSpec (Just (LinesIOSpec lines)) =
  P.byteStringInput . toS . T.unlines $ lines

matchOutput
  :: ByteString
  -> Maybe IOSpec
  -> ([Diff [String]] -> TestError)
  -> CommandMT ()
matchOutput _ Nothing _ = return ()
matchOutput actualBS (Just (RawIOSpec expectedT)) f =
  matchOutput actualBS (Just (LinesIOSpec $ T.lines expectedT)) f
matchOutput actualBS (Just (LinesIOSpec expectedLinesT)) f =
  let expectedLinesS = toS <$> expectedLinesT
      actualLinesS   = Str.lines (toS actualBS)
  in  case getGroupedDiff expectedLinesS actualLinesS of
        [] -> return ()
        d  -> throwError $ f d
