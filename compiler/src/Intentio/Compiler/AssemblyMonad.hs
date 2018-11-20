module Intentio.Compiler.AssemblyMonad
  ( resolveModule
  , resolveMainModule
  , resolveItem
  , moduleResolveItem
  )
where

import           Intentio.Prelude

import           Intentio.Compiler.Monad        ( CompileT
                                                , pushIce
                                                )
import           Intentio.Diagnostics           ( sourcePos )
import           Language.Intentio.Assembly     ( Assembly
                                                , ModuleName
                                                , ItemName
                                                , Module
                                                , ItemTy
                                                , lookupModule
                                                , lookupMainModule
                                                , moduleLookupItem
                                                , unModuleName
                                                , unItemName
                                                )

resolveModule :: (Module a, Monad m) => ModuleName -> Assembly a -> CompileT m a
resolveModule m a = case lookupModule m a of
  Just modul -> return modul
  Nothing    -> pushIce (() ^. sourcePos) err
  where err = "No module named \"" <> (m ^. unModuleName) <> "\"."
{-# INLINABLE resolveModule #-}

resolveMainModule :: (Module a, Monad m) => Assembly a -> CompileT m a
resolveMainModule a = case lookupMainModule a of
  Just modul -> return modul
  Nothing    -> pushIce (() ^. sourcePos) "No main module in this assembly."
{-# INLINABLE resolveMainModule #-}

resolveItem
  :: (Module a, Monad m)
  => ModuleName
  -> ItemName
  -> Assembly a
  -> CompileT m (a, ItemTy a)
resolveItem m i a = do
  modul <- resolveModule m a
  item  <- moduleResolveItem i modul
  return (modul, item)
{-# INLINABLE resolveItem #-}

moduleResolveItem
  :: (Module a, Monad m) => ItemName -> a -> CompileT m (ItemTy a)
moduleResolveItem i modul = do
  case modul ^. moduleLookupItem i of
    Just item -> return item
    Nothing   -> pushIce (modul ^. sourcePos) err
  where err = "This module has no item named \"" <> (i ^. unItemName) <> "\"."
{-# INLINABLE moduleResolveItem #-}
