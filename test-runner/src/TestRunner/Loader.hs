module TestRunner.Loader
  ( LoaderError
  , prettyLoaderError
  , loadTestSpec
  )
where

import           Intentio.Prelude

import           Data.Char                      ( isSpace )
import qualified Data.Text.Lazy                as TL
import qualified Data.Text.Lazy.IO             as TLIO
import           Data.Yaml                      ( ParseException
                                                , prettyPrintParseException
                                                , decodeEither'
                                                )
import           Data.Yaml.Include              ( decodeFileEither )

import           TestRunner.Model               ( TestCaseType(..)
                                                , TestCase
                                                , TestSpec
                                                , testCaseType
                                                , testCasePath
                                                )

data LoaderError
  = ParseException ParseException
  | NoSpecComment
  deriving (Show)

prettyLoaderError :: LoaderError -> Text
prettyLoaderError (ParseException p) = toS $ prettyPrintParseException p
prettyLoaderError NoSpecComment =
  "Source file does not contain test specification comment."

loadTestSpec :: TestCase -> IO (Either LoaderError TestSpec)
loadTestSpec testCase = case testCase ^. testCaseType of
  SingleFile -> loadSingle testCase
  MultiFile  -> loadMulti testCase

loadSingle :: TestCase -> IO (Either LoaderError TestSpec)
loadSingle testCase = do
  sourceLines <- TL.lines <$> TLIO.readFile (testCase ^. testCasePath)
  let commentLines = takeWhile (TL.isPrefixOf "#") sourceLines
  case commentLines of
    []       -> return $ Left NoSpecComment
    (hd : _) -> do
      let prefixLength = 1 + (TL.length . TL.takeWhile isSpace . TL.tail $ hd)
      let yamlText     = TL.unlines . fmap (TL.drop prefixLength) $ commentLines
      return . normalizeDecodeResult . decodeEither' . toS $ yamlText

loadMulti :: TestCase -> IO (Either LoaderError TestSpec)
loadMulti testCase =
  normalizeDecodeResult <$> decodeFileEither (testCase ^. testCasePath)

normalizeDecodeResult
  :: Either ParseException TestSpec -> Either LoaderError TestSpec
normalizeDecodeResult (Left  err) = Left $ ParseException err
normalizeDecodeResult (Right r  ) = Right r
