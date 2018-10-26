module Intentio.Util.NodeId
  ( NodeId
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

assign :: Annotated a => a x -> a NodeId
assign =
  flip evalState [mk 0 ..] . mapM (const $ state (unsafeFromJust . uncons))
{-# INLINABLE assign #-}
