module Intentio.Diagnostics
  ( DiagnosticPrintable(..)
  , DiagnosticPrintOpts(..)
  , diagnosticShow
  , DiagnosticSeverity(..)
  , Diagnostic(..)
  , diagnosticSeverity
  , diagnosticPos
  , diagnosticMessage
  , isDiagnosticErroneous
  , SourcePos(..)
  , sourcePos
  , initialSourcePos
  , sourceFile
  , sourceLine
  , sourceColumn
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
    = Warning
    | CompileError
    | InternalCompilerError
  deriving (Show, Read, Eq, Ord)

instance DiagnosticPrintable DiagnosticSeverity where
  diagnosticPrint _ Warning = "warning"
  diagnosticPrint _ CompileError = "error"
  diagnosticPrint _ InternalCompilerError = "ICE"

data Diagnostic = Diagnostic
  { _diagnosticSeverity :: DiagnosticSeverity
  , _diagnosticPos      :: SourcePos
  , _diagnosticMessage  :: Text
  } deriving (Show, Read, Eq, Ord)

instance DiagnosticPrintable Diagnostic where
  diagnosticPrint o (Diagnostic s p m) =
    diagnosticPrint o s <> ": " <> m <> "\n"
    <> " --> " <> diagnosticPrint o p

instance DiagnosticPrintable [Diagnostic] where
  diagnosticPrint o = (<> "\n")
                      . T.intercalate "\n\n"
                      . map (diagnosticPrint o)

class IsDiagnosticErroneous a where
  -- | States whether diagnostic is erroneous - i.e. compilation cannot
  -- be continued after it is issued.
  isDiagnosticErroneous :: a -> Bool

instance IsDiagnosticErroneous DiagnosticSeverity where
  isDiagnosticErroneous s = s > Warning

instance IsDiagnosticErroneous Diagnostic where
  isDiagnosticErroneous Diagnostic{_diagnosticSeverity} =
    isDiagnosticErroneous _diagnosticSeverity

instance IsDiagnosticErroneous [Diagnostic] where
  isDiagnosticErroneous = any isDiagnosticErroneous

----------------------------------------------------------------------------------
-- Source position
--
-- This part of code is inspired by Megaparsec's+-  SourcePos structure.

-- | Position in source file. Contains the name of the source file,
-- a line number, and a column number. Line and column numbers are 0-based
-- programmatically, but pretty printer returns 1-based numbers.
data SourcePos = SourcePos
  { _sourceFile   :: FilePath -- ^ Source file name
  , _sourceLine   :: !Word    -- ^ Line number
  , _sourceColumn :: !Word    -- ^ Column number
  } deriving (Show, Read, Eq, Ord)

-- | Construct source position, convenience shortcut for record syntax.
sourcePos :: FilePath -> Word -> Word -> SourcePos
sourcePos f !l !c =
  SourcePos {_sourceFile = f, _sourceLine = l, _sourceColumn = c}

-- | Construct initial source position (line 0, column 0) given source file name.
initialSourcePos :: FilePath -> SourcePos
initialSourcePos f = sourcePos f 0 0

instance DiagnosticPrintable SourcePos where
  diagnosticPrint _ (SourcePos f l c)
    | null f    = showLC
    | otherwise = toS f <> ":" <> showLC
    where showLC = show (l + 1) <> ":" <> show (c + 1)

----------------------------------------------------------------------------------
-- Lenses

makeLenses ''Diagnostic
makeLenses ''SourcePos
