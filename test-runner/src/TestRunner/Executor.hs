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

import           TestRunner.Loader              ( LoaderError
                                                , prettyLoaderError
                                                , loadTestSpec
                                                )
import           TestRunner.Model               ( TestCase
                                                , TestSpec
                                                , TestCommand(..)
                                                , commands
                                                )
import           TestRunner.Opts                ( Opts )

data TestError = LoaderError LoaderError
  deriving (Show)

data TestResult = TestResult
  { _testResultCase :: TestCase
  , _testResultState :: Either TestError ()
  }
  deriving (Show)

newtype CommandState = CommandState
  { _compiledBinary :: Maybe FilePath
  }

type RunSpecMT = ExceptT TestError IO
type CommandMT = StateT CommandState RunSpecMT

makeLenses ''TestResult
makeLenses ''CommandState

prettyTestError :: TestError -> Text
prettyTestError (LoaderError e) = prettyLoaderError e

runTestCase :: Opts -> TestCase -> IO TestResult
runTestCase opts testCase = do
  let _testResultCase = testCase
  _testResultState <-
    runExceptT $ loadTestSpec' testCase >>= runTestSpec opts testCase
  return TestResult {..}

loadTestSpec' :: TestCase -> RunSpecMT TestSpec
loadTestSpec' testCase =
  testCase & loadTestSpec & liftIO <&> (_Left %~ LoaderError) >>= liftEither

runTestSpec :: Opts -> TestCase -> TestSpec -> RunSpecMT ()
runTestSpec opts testCase spec = void $ execStateT
  (mapM_ (runCommand opts testCase) (spec ^. commands))
  emptyCommandState

emptyCommandState :: CommandState
emptyCommandState = CommandState {_compiledBinary = Nothing}

runCommand :: Opts -> TestCase -> TestCommand -> CommandMT ()
runCommand opts testCase (CompileCommand spec) = return ()
runCommand opts testCase (RunCommand     spec) = return ()
