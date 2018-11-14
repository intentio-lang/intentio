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
mangle ps = "_ZI" <> T.concat (fmap f ps) <> "E"
  where f p = let s = sanitize p in show (T.length s) <> s

-- FIXME: Use Itanium C++ ABI Name Mangling algorithm
sanitize :: Text -> Text
sanitize = underscoreQualify . T.concatMap f
 where
  f '_' = "_U_"
  f '-' = "_P_"
  f ',' = "_C_"
  f ':' = "_M_"
  f '.' = "_D_"
  f '(' = "_LP_"
  f ')' = "_RP_"
  f '@' = "_SP_"
  f '*' = "_BP_"
  f '/' = "_S_"
  f '&' = "_RF_"
  f '%' = "_PC_"
  f '<' = "_LT_"
  f '>' = "_GT_"
  f '$' = "_DL_"
  f c | isAsciiLower c = T.singleton c
      | isAsciiUpper c = T.singleton c
      | isDigit c      = T.singleton c
      | otherwise      = "_x" <> toS (showHex (ord c) "_")

  underscoreQualify "" = ""
  underscoreQualify s@(T.head -> c) | c == '_'     = s
                                    | isAlphaNum c = s
                                    | otherwise    = '_' <| s
