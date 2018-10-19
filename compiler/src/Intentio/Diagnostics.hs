module Intentio.Diagnostics
  ( module Language.Intentio.SourcePos

    -- * Compiler diagnostics
  , Diagnostic(..)
  , diagnosticSeverity
  , diagnosticPos
  , diagnosticMessage
  , diagnosticFor
  , cnote
  , cnoteFor
  , chint
  , chintFor
  , cwarning
  , cwarningFor
  , cerror
  , cerrorFor
  , ice
  , iceFor
  , DiagnosticSeverity(..)
  , DiagnosticPrintable(..)
  , DiagnosticPrintOpts(..)
  , diagnosticShow
  )
where

import           Intentio.Prelude        hiding ( sourceFile
                                                , sourceLine
                                                , sourceColumn
                                                )

import qualified Data.Text                     as T

import           Language.Intentio.SourcePos

----------------------------------------------------------------------------------
-- Diagnostic data type

data DiagnosticPrintOpts = DiagnosticPrintOpts
  deriving (Show, Read, Eq, Ord)

class DiagnosticPrintable a where
  -- | Print value for use in diagnostic message. Must respect options.
  diagnosticPrint :: DiagnosticPrintOpts -> a -> Text

-- | Print value for use in diagnostic message with raw formatting options.
diagnosticShow :: DiagnosticPrintable a => a -> Text
diagnosticShow = diagnosticPrint DiagnosticPrintOpts

data DiagnosticSeverity
    = DiagnosticNote
    | DiagnosticHint
    | DiagnosticWarning
    | DiagnosticError
    | DiagnosticICE
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

instance DiagnosticPrintable DiagnosticSeverity where
  diagnosticPrint _ DiagnosticNote    = "note"
  diagnosticPrint _ DiagnosticHint    = "hint"
  diagnosticPrint _ DiagnosticWarning = "warning"
  diagnosticPrint _ DiagnosticError   = "error"
  diagnosticPrint _ DiagnosticICE     = "internal compiler error"

data Diagnostic = Diagnostic
  { _diagnosticSeverity :: DiagnosticSeverity
  , _diagnosticPos      :: SourcePos
  , _diagnosticMessage  :: Text
  } deriving (Show, Read, Eq, Ord)

instance DiagnosticPrintable Diagnostic where
  diagnosticPrint o (Diagnostic sev pos msg) =
    diagnosticPrint o pos
      <> ": "
      <> diagnosticPrint o sev
      <> ":\n"
      <> fmtMsg
      <> "\n"
   where
    indent = "   "
    fmtMsg = indent <> T.replace "\n" ("\n" <> indent) msg

instance DiagnosticPrintable [Diagnostic] where
  diagnosticPrint o = T.intercalate "\n" . fmap (diagnosticPrint o)

instance DiagnosticPrintable (Seq Diagnostic) where
  diagnosticPrint o = diagnosticPrint o . toList

instance DiagnosticPrintable SourcePos where
  diagnosticPrint _ (SourcePos "" 0 0) = "?"
  diagnosticPrint _ (SourcePos f l c) | null f    = showLC
                                      | otherwise = toS f <> ":" <> showLC
    where showLC = show (l + 1) <> ":" <> show (c + 1)

diagnosticFor :: HasSourcePos a => DiagnosticSeverity -> a -> Text -> Diagnostic
diagnosticFor s p = Diagnostic s (_sourcePos p)
{-# INLINE diagnosticFor #-}

cnote :: SourcePos -> Text -> Diagnostic
cnote = Diagnostic DiagnosticNote
{-# INLINE cnote #-}

cnoteFor :: HasSourcePos a => a -> Text -> Diagnostic
cnoteFor = diagnosticFor DiagnosticNote
{-# INLINE cnoteFor #-}

chint :: SourcePos -> Text -> Diagnostic
chint = Diagnostic DiagnosticHint
{-# INLINE chint #-}

chintFor :: HasSourcePos a => a -> Text -> Diagnostic
chintFor = diagnosticFor DiagnosticHint
{-# INLINE chintFor #-}

cwarning :: SourcePos -> Text -> Diagnostic
cwarning = Diagnostic DiagnosticWarning
{-# INLINE cwarning #-}

cwarningFor :: HasSourcePos a => a -> Text -> Diagnostic
cwarningFor = diagnosticFor DiagnosticWarning
{-# INLINE cwarningFor #-}

cerror :: SourcePos -> Text -> Diagnostic
cerror = Diagnostic DiagnosticError
{-# INLINE cerror #-}

cerrorFor :: HasSourcePos a => a -> Text -> Diagnostic
cerrorFor = diagnosticFor DiagnosticError
{-# INLINE cerrorFor #-}

ice :: SourcePos -> Text -> Diagnostic
ice = Diagnostic DiagnosticICE
{-# INLINE ice #-}

iceFor :: HasSourcePos a => a -> Text -> Diagnostic
iceFor = diagnosticFor DiagnosticICE
{-# INLINE iceFor #-}

----------------------------------------------------------------------------------
-- Lenses

makeLenses ''Diagnostic
