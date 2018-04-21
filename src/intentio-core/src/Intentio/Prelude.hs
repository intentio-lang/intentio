module Intentio.Prelude
  ( module P
  , module S
  )
where

import           Prelude                       as P
                                                ( String )

import           Protolude                     as P
                                         hiding ( MonadState
                                                , State
                                                , StateT(StateT)
                                                , put
                                                , get
                                                , gets
                                                , modify
                                                , state
                                                , withState
                                                , runState
                                                , execState
                                                , evalState
                                                , runStateT
                                                , execStateT
                                                , evalStateT
                                                )

import           Control.Monad.State.Strict    as S
                                                ( MonadState
                                                , State
                                                , StateT(StateT)
                                                , put
                                                , get
                                                , gets
                                                , modify
                                                , modify'
                                                , state
                                                , withState
                                                , runState
                                                , execState
                                                , evalState
                                                , runStateT
                                                , execStateT
                                                , evalStateT
                                                )
