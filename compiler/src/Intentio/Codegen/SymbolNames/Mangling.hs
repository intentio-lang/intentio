module Intentio.Codegen.SymbolNames.Mangling
  ( mangle
  , sanitize
  )
where

import           Intentio.Prelude

import           Data.Char                      ( isAlphaNum
                                                , isAsciiLower
                                                , isAsciiUpper
                                                , isDigit
                                                , ord
                                                )
import qualified Data.Text                     as T
import           Numeric                        ( showHex )

mangle :: [Text] -> Text
mangle ps = "_ZN" <> T.concat (fmap f ps) <> "E"
  where f p = let s = sanitize p in show (T.length s) <> s

sanitize :: Text -> Text
sanitize = underscoreQualify . T.concatMap f
 where
  f '@' = "$SP$"
  f '*' = "$BP$"
  f '&' = "$RF$"
  f '<' = "$LT$"
  f '>' = "$GT$"
  f '(' = "$LP$"
  f ')' = "$RP$"
  f ',' = "$C$"
  f '_' = "_"
  f '.' = "."
  f '$' = "$"
  f c | isAsciiLower c = T.singleton c
      | isAsciiUpper c = T.singleton c
      | isDigit c      = T.singleton c
      | otherwise      = "$" <> toS (showHex (ord c) "$")

  underscoreQualify "" = ""
  underscoreQualify s@(T.head -> c) | c == '_'     = s
                                    | isAlphaNum c = s
                                    | otherwise    = '_' <| s
