module Language.Intentio.SourcePos
  ( LineNumber
  , ColumnNumber
  , SourcePos(..)
  , sourceFile
  , sourceLine
  , sourceColumn
  , HasSourcePos(..)
  , sourcePos
  )
where

import           Intentio.Prelude        hiding ( sourceFile
                                                , sourceLine
                                                , sourceColumn
                                                )

import           Data.Aeson                     ( ToJSON(..)
                                                , FromJSON(..)
                                                )
import qualified Text.Megaparsec               as MP
import qualified Text.Megaparsec.Pos           as MPP

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
  } deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON SourcePos where
  toJSON (SourcePos f l c) = toJSON (f, l, c)

instance FromJSON SourcePos where
  parseJSON v = do
    (f, l, c) <- parseJSON v
    return $ SourcePos f l c

class HasSourcePos a where
  -- | Get source position of given item
  _sourcePos :: a -> SourcePos

sourcePos
  :: (Profunctor p, Contravariant f, HasSourcePos a) => Optic' p f a SourcePos
sourcePos = to _sourcePos

instance HasSourcePos () where
  _sourcePos () = SourcePos "" 0 0
  {-# INLINABLE _sourcePos #-}

instance HasSourcePos Void where
  _sourcePos _ = SourcePos "" 0 0
  {-# INLINABLE _sourcePos #-}

instance HasSourcePos SourcePos where
  _sourcePos = id
  {-# INLINE _sourcePos #-}

instance HasSourcePos (MP.State s) where
  _sourcePos = _sourcePos . MP.statePosState

instance HasSourcePos (MP.PosState s) where
  _sourcePos = _sourcePos . MP.pstateSourcePos

instance HasSourcePos MPP.SourcePos where
  _sourcePos (MPP.SourcePos n l c) = SourcePos n (unPos l) (unPos c)
    where unPos p = fromIntegral $ MPP.unPos p - 1

----------------------------------------------------------------------------------
-- Lenses

makeLenses ''SourcePos
