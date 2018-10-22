module Intentio.Hir.Resolver
  ( NodeId
  , mkNodeId
  , nullNodeId
  , assignNodeIds
  )
where

import           Intentio.Prelude

import           Intentio.Annotated             ( Annotated(..)
                                                , ann
                                                )
import qualified Intentio.Hir.Model            as H
import qualified Language.Intentio.AST         as A

newtype NodeId = NodeId (Maybe Word)
  deriving (Eq, Ord, Read, Show)

instance Enum NodeId where
  toEnum = mkNodeId
  {-# INLINE toEnum #-}

  fromEnum (NodeId Nothing)  = -1
  fromEnum (NodeId (Just x)) = fromEnum x
  {-# INLINE fromEnum #-}

mkNodeId :: Int -> NodeId
mkNodeId n | n < 0     = nullNodeId
           | otherwise = NodeId . Just . toEnum $ n
{-# INLINE mkNodeId #-}

nullNodeId :: NodeId
nullNodeId = NodeId Nothing
{-# INLINE nullNodeId #-}

assignNodeIds :: forall a . Annotated a => a () -> a NodeId
assignNodeIds =
  flip evalState [mkNodeId 0 ..]
    . mapM (const $ state (unsafeFromJust . uncons))
    . fmap (const nullNodeId)
