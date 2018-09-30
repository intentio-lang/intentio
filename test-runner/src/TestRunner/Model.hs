module TestRunner.Model where

import           Intentio.Prelude

import           Data.Aeson.Types               ( typeMismatch )
import qualified Data.HashMap.Strict           as HM
import           Data.Yaml                      ( FromJSON(..)
                                                , Value(..)
                                                , (.:?)
                                                , withArray
                                                , withObject
                                                )

newtype TestSuite = TestSuite { _testCases :: [TestCase] }
  deriving (Show, Eq)

data TestCaseType = MultiFile | SingleFile
  deriving (Show, Eq)

data TestCase = TestCase
  { _testCaseType :: TestCaseType
  , _testCaseName :: Text
  , _testCasePath :: FilePath
  }
  deriving (Show, Eq)

newtype TestSpec = TestSpec { _commands :: [TestCommand] }
  deriving (Show, Eq)

data TestCommand
  = CompileCommand CompileCommandSpec
  | RunCommand RunCommandSpec
  deriving (Show, Eq)

newtype CompileCommandSpec = CompileCommandSpec { _compileArgs :: Arguments }
  deriving (Show, Eq)

newtype Arguments = Arguments { _argv :: [Text] }
  deriving (Show, Eq)

data RunCommandSpec = RunCommandSpec
  { _stdin  :: Maybe IOSpec
  , _stdout :: Maybe IOSpec
  , _stderr :: Maybe IOSpec
  }
  deriving (Show, Eq)

data IOSpec = RawIOSpec Text | LinesIOSpec [Text]
  deriving (Show, Eq)

makeLenses ''TestSuite
makeLenses ''TestCase
makeLenses ''TestSpec
makePrisms ''TestCommand
makeLenses ''CompileCommandSpec
makeLenses ''Arguments
makeLenses ''RunCommandSpec
makePrisms ''IOSpec

instance FromJSON TestSpec where
  parseJSON = withArray "TestSpec" $ fmap (TestSpec . toList) . mapM parseJSON

instance FromJSON TestCommand where
  parseJSON = withObject "TestCommand" $ \o -> do
    (k, v) <- case HM.toList o of
      [x] -> return x
      _   -> fail "Command object must have exactly one key."
    case k of
      "compile" -> CompileCommand <$> parseJSON v
      "run"     -> RunCommand <$> parseJSON v
      unknown   -> fail . toS $ "Unknown command " <> unknown

deriving instance FromJSON CompileCommandSpec

instance FromJSON Arguments where
  parseJSON = withArray "Arguments" $ fmap (Arguments . toList) . mapM parseJSON

instance FromJSON RunCommandSpec where
  parseJSON = withObject "RunCommandSpec" $ \o ->
    RunCommandSpec <$> o .:? "stdin" <*> o .:? "stdout" <*> o .:? "stderr"

instance FromJSON IOSpec where
  parseJSON (String t) = return $ RawIOSpec t
  parseJSON (Array a)  = fmap (LinesIOSpec . toList) . mapM parseJSON $ a
  parseJSON invalid    = typeMismatch "IOSpec" invalid
