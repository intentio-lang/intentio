module Intentio.Util.NodeId
  ( NodeId
  , mk
  , null
  , assign
  )
where

import           Intentio.Prelude        hiding ( assign
                                                , null
                                                )

import           Intentio.Annotated             ( Annotated(..) )

newtype NodeId = NodeId (Maybe Word)
  deriving (Eq, Ord, Read, Show)

instance Enum NodeId where
  toEnum = mk
  {-# INLINE toEnum #-}

  fromEnum (NodeId Nothing ) = -1
  fromEnum (NodeId (Just x)) = fromEnum x
  {-# INLINE fromEnum #-}

mk :: Int -> NodeId
mk n | n < 0     = null
     | otherwise = NodeId . Just . toEnum $ n
{-# INLINE mk #-}

null :: NodeId
null = NodeId Nothing
{-# INLINE null #-}

assign :: forall a . Annotated a => a () -> a NodeId
assign =
  flip evalState [mk 0 ..]
    . mapM (const $ state (unsafeFromJust . uncons))
    . fmap (const null)
