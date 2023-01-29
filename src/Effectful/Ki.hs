{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Effectful.Ki (
    -- * Effect
    StructuredConcurrency,

    -- * Handlers
    runStructuredConcurrency,

    -- * Core API
    Scope,
    Thread,
    scoped,
    fork,
    forkTry,
    await,
    awaitAll,

    -- * Extended API
    fork_,
    forkWith,
    forkWith_,
    forkTryWith,

    -- ** Thread options
    ThreadOptions (..),
    defaultThreadOptions,
    ThreadAffinity (..),

    -- ** Byte count
    ByteCount,
    kilobytes,
    megabytes,

    -- * STM re-export
    Effectful.Ki.atomically,
    Effectful.Ki.newTVarIO,
    Effectful.Ki.newTMVarIO,
    Effectful.Ki.newEmptyTMVarIO,
) where

import Control.Concurrent.STM hiding (atomically)
import Control.Concurrent.STM qualified as STM
import Control.Exception (Exception)
import Data.Void (Void)
import Effectful
import Effectful.Dispatch.Static
import Effectful.Dispatch.Static.Primitive (cloneEnv)
import Effectful.Dispatch.Static.Unsafe (reallyUnsafeUnliftIO)

import Ki hiding (fork, forkTry, forkTryWith, forkWith, forkWith_, fork_, scoped)
import Ki qualified

data StructuredConcurrency :: Effect

type instance DispatchOf StructuredConcurrency = 'Static 'WithSideEffects
data instance StaticRep StructuredConcurrency = StructuredConcurrency

-- | Run the 'StructuredConcurrency' effect.
runStructuredConcurrency :: IOE :> es => Eff (StructuredConcurrency : es) a -> Eff es a
runStructuredConcurrency = evalStaticRep StructuredConcurrency

scoped :: StructuredConcurrency :> es => (Scope -> Eff es a) -> Eff es a
scoped action = reallyUnsafeUnliftIO $ \runInIO ->
    Ki.scoped $ \scope ->
        runInIO $ action scope

fork ::
    StructuredConcurrency :> es =>
    Scope ->
    Eff es a ->
    Eff es (Thread a)
fork scope action = do
    unsafeEff $ \es -> do
        es' <- cloneEnv es
        Ki.fork scope (unEff action es')

forkTry ::
    Exception e =>
    StructuredConcurrency :> es =>
    Scope ->
    Eff es a ->
    Eff es (Thread (Either e a))
forkTry scope action = do
    unsafeEff $ \es -> do
        es' <- cloneEnv es
        Ki.forkTry scope (unEff action es')

fork_ ::
    StructuredConcurrency :> es =>
    Scope ->
    Eff es Void ->
    Eff es ()
fork_ scope action = do
    unsafeEff $ \es -> do
        es' <- cloneEnv es
        Ki.fork_ scope (unEff action es')

forkWith ::
    StructuredConcurrency :> es =>
    Scope ->
    ThreadOptions ->
    Eff es a ->
    Eff es (Thread a)
forkWith scope threadOptions action = do
    unsafeEff $ \es -> do
        es' <- cloneEnv es
        Ki.forkWith scope threadOptions (unEff action es')

forkWith_ ::
    StructuredConcurrency :> es =>
    Scope ->
    ThreadOptions ->
    Eff es Void ->
    Eff es ()
forkWith_ scope threadOptions action = do
    unsafeEff $ \es -> do
        es' <- cloneEnv es
        Ki.forkWith_ scope threadOptions (unEff action es')

forkTryWith ::
    Exception e =>
    Scope ->
    StructuredConcurrency :> es =>
    ThreadOptions ->
    Eff es a ->
    Eff es (Thread (Either e a))
forkTryWith scope threadOptions action = do
    unsafeEff $ \es -> do
        es' <- cloneEnv es
        Ki.forkTryWith scope threadOptions (unEff action es')

atomically :: StructuredConcurrency :> es => STM a -> Eff es a
atomically = unsafeEff_ . STM.atomically

newTVarIO :: StructuredConcurrency :> es => a -> Eff es (TVar a)
newTVarIO = unsafeEff_ . STM.newTVarIO

newTMVarIO :: StructuredConcurrency :> es => a -> Eff es (TMVar a)
newTMVarIO = unsafeEff_ . STM.newTMVarIO

newEmptyTMVarIO :: StructuredConcurrency :> es => Eff es (TMVar a)
newEmptyTMVarIO = unsafeEff_ STM.newEmptyTMVarIO
