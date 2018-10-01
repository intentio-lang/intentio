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
import           Control.Monad.Morph            ( hoist )
import           Data.Algorithm.Diff            ( Diff(..)
                                                , getGroupedDiff
                                                )
import           Data.Algorithm.DiffOutput      ( ppDiff )
import qualified Data.String                   as Str
import qualified Data.Text                     as T
import           System.Directory               ( withCurrentDirectory )
import           System.IO.Temp                 ( withSystemTempDirectory )
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
                                                )
import           TestRunner.Opts                ( Opts
                                                , compilerPath
                                                , runEntrypointPath
                                                )

data TestError
  = LoaderError LoaderError
  | CompilationFailure Int ByteString
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
  , _compiledBinary :: Maybe FilePath
  }

type RunSpecMT = ExceptT TestError IO
type CommandMT = StateT CommandState RunSpecMT

makeLenses ''TestResult
makeLenses ''CommandState

outputBinaryPath :: FilePath
outputBinaryPath = "testbin.out"

prettyTestError :: TestError -> Text
prettyTestError (LoaderError e            ) = prettyLoaderError e
prettyTestError (CompilationFailure ec out) = T.unlines $ [ecs, ""] <> outs
 where
  ecs  = "Compilation failed with exit code: " <> show ec
  outs = T.lines $ toS out
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
  _testResultState <- runExceptT $ loadTestSpec' testCase >>= runTestSpec opts
  return TestResult { .. }

loadTestSpec' :: TestCase -> RunSpecMT TestSpec
loadTestSpec' testCase =
  testCase & loadTestSpec & liftIO <&> (_Left %~ LoaderError) >>= liftEither

runTestSpec :: Opts -> TestSpec -> RunSpecMT ()
runTestSpec opts spec = do
  withSystemTempDirectory "intentio-test" $ \cwd ->
    hoist (withCurrentDirectory cwd) $ do
      void $ execStateT (mapM_ runCommand (spec ^. commands))
                        (emptyCommandState opts)

emptyCommandState :: Opts -> CommandState
emptyCommandState _stateOpts = CommandState { _compiledBinary = Nothing, .. }

runCommand :: TestCommand -> CommandMT ()
runCommand (CompileCommand spec) = do
  myCompilerPath <- use (stateOpts . compilerPath)
  let myCompilerArgs =
        ["-o", outputBinaryPath] <> (spec ^. compileCommandArgs . argv <&> toS)
  (exitCode, compilerStdout) <- P.readProcessStdout
    $ P.proc myCompilerPath myCompilerArgs
  case exitCode of
    ExitSuccess    -> compiledBinary .= Just outputBinaryPath
    ExitFailure ec -> throwError $ CompilationFailure ec (toS compilerStdout)

runCommand (RunCommand spec) = do
  myRunEntrypointPath <- use (stateOpts . runEntrypointPath)
  myCompiledBinary    <- use compiledBinary >>= \case
    Just p  -> return p
    Nothing -> throwError RunCalledWithoutCompile
  let procPath = decorateProcPath myRunEntrypointPath (myCompiledBinary, [])
  let procConf =
        P.setStdin (buildStdinSpec $ spec ^. runCommandStdin) $ mkProc procPath
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
