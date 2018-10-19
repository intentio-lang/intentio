module Intentio.Annotated
  ( Annotated(..)
  )
where

import           Intentio.Prelude

class Annotated (a :: * -> *) where
  getAnn :: a b -> b
  getAnn a = a ^. ann

  setAnn :: b -> a b -> a b
  setAnn b a = a & ann .~ b

  ann :: Lens' (a b) b
  ann = lens getAnn (flip setAnn)

  {-# MINIMAL getAnn, setAnn | ann #-}
