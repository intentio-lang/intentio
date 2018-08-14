module Intentio.Diagnostics
  ( -- * Compiler diagnostics
    Diagnostic(..)
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

    -- * Source position
  , LineNumber
  , ColumnNumber
  , SourcePos(..)
  , sourceFile
  , sourceLine
  , sourceColumn
  , SourcePosProvider(..)
  )
where

import           Intentio.Prelude        hiding ( sourceFile
                                                , sourceLine
                                                , sourceColumn
                                                )

import qualified Data.Text                     as T

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
  diagnosticPrint _ DiagnosticNote = "note"
  diagnosticPrint _ DiagnosticHint = "hint"
  diagnosticPrint _ DiagnosticWarning = "warning"
  diagnosticPrint _ DiagnosticError = "error"
  diagnosticPrint _ DiagnosticICE = "ICE"

data Diagnostic = Diagnostic
  { _diagnosticSeverity :: DiagnosticSeverity
  , _diagnosticPos      :: SourcePos
  , _diagnosticMessage  :: Text
  } deriving (Show, Read, Eq, Ord)

instance DiagnosticPrintable Diagnostic where
  diagnosticPrint o (Diagnostic sev pos msg) =
    diagnosticPrint o pos <> ": " <> diagnosticPrint o sev <> ":\n"
    <> fmtMsg <> "\n"
    where
      indent = "   "
      fmtMsg = indent <> T.replace "\n" ("\n" <> indent) msg

instance DiagnosticPrintable [Diagnostic] where
  diagnosticPrint o =  T.intercalate "\n" . map (diagnosticPrint o)

diagnosticFor
  :: SourcePosProvider a => DiagnosticSeverity -> a -> Text -> Diagnostic
diagnosticFor s p m = Diagnostic s (sourcePos p) m
{-# INLINE diagnosticFor #-}

cnote :: SourcePos -> Text -> Diagnostic
cnote = Diagnostic DiagnosticNote
{-# INLINE cnote #-}

cnoteFor :: SourcePosProvider a => a -> Text -> Diagnostic
cnoteFor = diagnosticFor DiagnosticNote
{-# INLINE cnoteFor #-}

chint :: SourcePos -> Text -> Diagnostic
chint = Diagnostic DiagnosticHint
{-# INLINE chint #-}

chintFor :: SourcePosProvider a => a -> Text -> Diagnostic
chintFor = diagnosticFor DiagnosticHint
{-# INLINE chintFor #-}

cwarning :: SourcePos -> Text -> Diagnostic
cwarning = Diagnostic DiagnosticWarning
{-# INLINE cwarning #-}

cwarningFor :: SourcePosProvider a => a -> Text -> Diagnostic
cwarningFor = diagnosticFor DiagnosticWarning
{-# INLINE cwarningFor #-}

cerror :: SourcePos -> Text -> Diagnostic
cerror = Diagnostic DiagnosticError
{-# INLINE cerror #-}

cerrorFor :: SourcePosProvider a => a -> Text -> Diagnostic
cerrorFor = diagnosticFor DiagnosticError
{-# INLINE cerrorFor #-}

ice :: SourcePos -> Text -> Diagnostic
ice = Diagnostic DiagnosticICE
{-# INLINE ice #-}

iceFor :: SourcePosProvider a => a -> Text -> Diagnostic
iceFor = diagnosticFor DiagnosticICE
{-# INLINE iceFor #-}

----------------------------------------------------------------------------------
-- Source position
--
-- This part of code is inspired by Megaparsec's SourcePos structure.

type LineNumber = Word
type ColumnNumber = Word

-- | Position in source file. Contains the name of the source file,
-- a line number, and a column number. Line and column numbers are 0-based
-- programmatically, but pretty printer returns 1-based numbers.
data SourcePos = SourcePos
  { _sourceFile   :: FilePath       -- ^ Source file name
  , _sourceLine   :: !LineNumber    -- ^ Line number
  , _sourceColumn :: !ColumnNumber  -- ^ Column number
  } deriving (Show, Read, Eq, Ord)

instance DiagnosticPrintable SourcePos where
  diagnosticPrint _ (SourcePos "" 0 0) = "?"
  diagnosticPrint _ (SourcePos f l c)
    | null f    = showLC
    | otherwise = toS f <> ":" <> showLC
    where showLC = show (l + 1) <> ":" <> show (c + 1)

class SourcePosProvider a where
  -- | Construct source position for given item
  sourcePos :: a -> SourcePos

instance SourcePosProvider () where
  sourcePos () = SourcePos "" 0 0
  {-# INLINABLE sourcePos #-}

instance SourcePosProvider SourcePos where
  sourcePos = id
  {-# INLINE sourcePos #-}

----------------------------------------------------------------------------------
-- Lenses

makeLenses ''Diagnostic
makeLenses ''SourcePos
