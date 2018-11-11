module Intentio.Util.NodeId
  ( NodeId
  , HasNodeId(..)
  , mk
  , assign
  )
where

import           Intentio.Prelude        hiding ( assign )

import           Intentio.Annotated             ( Annotated(..) )

newtype NodeId = NodeId Word
  deriving (Bounded, Enum, Eq, Ord, Read, Show, ToJSON, FromJSON)

mk :: Int -> NodeId
mk = toEnum
{-# INLINE mk #-}

assign :: (Annotated a, Traversable a) => a x -> a NodeId
assign =
  flip evalState [mk 0 ..] . mapM (const $ state (unsafeFromJust . uncons))
{-# INLINABLE assign #-}

class HasNodeId a where
  nodeId :: Lens' a NodeId

instance HasNodeId NodeId where
  nodeId = id
  {-# INLINE nodeId #-}

instance HasNodeId (NodeId, a) where
  nodeId = _1
  {-# INLINE nodeId #-}

instance Annotated a => HasNodeId (a NodeId) where
  nodeId = ann
  {-# INLINE nodeId #-}

instance Annotated a => HasNodeId (a (NodeId, x)) where
  nodeId = ann . _1
  {-# INLINE nodeId #-}
