module Intentio.Interactive
  ( runInteractive
  )
where

import qualified Prelude                       as P
import           Intentio.Prelude

import           Data.Char                      ( isSpace )
import           Data.String                    ( fromString )
import qualified Data.Text                     as T

import           System.Console.Haskeline

import           Language.Intentio.Lexer        ( lexTest' )

--------------------------------------------------------------------------------
-- runInteractive

runInteractive :: IO ()
runInteractive = evalStateT (runInputT settings monad) initialReplState
 where
  settings = defaultSettings

  monad    = do
    outputStrLn "Welcome to Intentio Interactive!"
    outputStrLn "Type :help for help, and type :q or press ^D to exit."
    outputStrLn ""
    loop
    outputStrLn "Goodbye!"

  loop :: Repl ()
  loop = do
    currMode <- lift $ gets mode
    input'   <- getInputLine $ blue (show currMode ++ "> ")
    case input' of
      Nothing    -> return ()
      Just ":q"  -> return ()
      Just input -> (processInput . T.pack $ input) >> loop

  processInput :: Text -> Repl ()
  processInput input = case parse (trimStart input) of
    (Just cmd, _ ) -> evaluateCmd cmd
    (Nothing , "") -> return ()
    (Nothing , _ ) -> outputStrLn $ red "invalid command"

--
-- Data structures
--

class Parse a where
  parse :: Text -> (Maybe a, Text)

  parseFull :: Text -> Maybe a
  parseFull s = case parse s of
    (result, rest) | rest == fromString "" -> result
    _ -> Nothing

data Cmd
  = AstCmd Text
  | EvalCmd Text
  | HelpCmd
  | LexCmd Text
  | SetModeCmd Text
  | CurrModeCmd Text
  deriving (Show)

instance Parse Cmd where
  parse (T.stripPrefix ":ast " -> Just r) = (Just $ AstCmd (trimStart r), "")

  parse (T.words -> [":help"]) = (Just HelpCmd, "")

  parse (T.stripPrefix ":lex" -> Just r) = (Just $ LexCmd (trimStart r), "")

  parse (T.words -> [":mode", modeName]) = (Just $ SetModeCmd modeName, "")

  parse r
    | trimStart r == "" = (Nothing, "")
    | T.head r == ':'   = (Nothing, r)
    | otherwise         = (Just $ CurrModeCmd r, "")

data ReplMode = AstMode | EvalMode | LexMode

mode2cmd :: ReplMode -> (Text -> Cmd)
mode2cmd AstMode  = AstCmd
mode2cmd EvalMode = EvalCmd
mode2cmd LexMode  = LexCmd

instance P.Show ReplMode where
  show AstMode  = "ast"
  show EvalMode = "intentio"
  show LexMode  = "lex"

instance Parse ReplMode where
  parse (T.stripPrefix "ast"      -> Just r) = (Just AstMode, r)
  parse (T.stripPrefix "eval"     -> Just r) = (Just EvalMode, r)
  parse (T.stripPrefix "intentio" -> Just r) = (Just EvalMode, r)
  parse (T.stripPrefix "lex"      -> Just r) = (Just LexMode, r)
  parse r                                    = (Nothing, r)

newtype ReplState = ReplState {
  mode :: ReplMode
}

initialReplState :: ReplState
initialReplState = ReplState {mode = EvalMode}

type Repl a = InputT (StateT ReplState IO) a

--------------------------------------------------------------------------------
-- Command evaluation

evaluateCmd :: Cmd -> Repl ()

evaluateCmd (CurrModeCmd t) = do
  currMode <- lift $ gets mode
  evaluateCmd . mode2cmd currMode $ t

evaluateCmd HelpCmd = do
  outputStrLn "TODO: Help"

evaluateCmd (SetModeCmd modeName) = case parseFull modeName of
  Just mode -> lift . modify $ \s -> s { mode }
  Nothing   -> outputStrLn $ red "unknown mode"

evaluateCmd (LexCmd input) = liftIO $ lexTest' input

evaluateCmd c              = outputStrLn $ "TODO: " ++ show c

--------------------------------------------------------------------------------
-- Utilities

trimStart :: Text -> Text
trimStart = T.dropWhile isSpace

blue, red :: String -> String
blue s = "\ESC[1;34m\STX" ++ s ++ "\ESC[0m\STX"
red s = "\ESC[1;31m\STX" ++ s ++ "\ESC[0m\STX"
