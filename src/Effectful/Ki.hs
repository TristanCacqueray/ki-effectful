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

-- import Effectful.Dispatch.Static.Primitive (cloneEnv)

import Ki hiding (fork, forkTry, fork_, scoped)
import Ki qualified

data StructuredConcurrency :: Effect

type instance DispatchOf StructuredConcurrency = 'Static 'WithSideEffects
data instance StaticRep StructuredConcurrency = StructuredConcurrency

-- | Run the 'StructuredConcurrency' effect.
runStructuredConcurrency :: IOE :> es => Eff (StructuredConcurrency : es) a -> Eff es a
runStructuredConcurrency = evalStaticRep StructuredConcurrency

scoped ::
    StructuredConcurrency :> es =>
    (Scope -> Eff es a) ->
    Eff es a
scoped action = unsafeEff $ \es -> do
    Ki.scoped (\scope -> unEff (action scope) es)

fork ::
    StructuredConcurrency :> es =>
    Scope ->
    Eff es a ->
    Eff es (Thread a)
fork scope action = unsafeEff $ \es -> do
    Ki.fork scope (unEff action es)

forkTry ::
    Exception e =>
    StructuredConcurrency :> es =>
    Scope ->
    Eff es a ->
    Eff es (Thread (Either e a))
forkTry scope action = unsafeEff $ \es -> do
    Ki.forkTry scope (unEff action es)

fork_ ::
    StructuredConcurrency :> es =>
    Scope ->
    Eff es Void ->
    Eff es ()
fork_ scope action = unsafeEff $ \es -> do
    Ki.fork_ scope (unEff action es)

atomically :: StructuredConcurrency :> es => STM a -> Eff es a
atomically = unsafeEff_ . STM.atomically

newTVarIO :: StructuredConcurrency :> es => a -> Eff es (TVar a)
newTVarIO = unsafeEff_ . STM.newTVarIO

newTMVarIO :: StructuredConcurrency :> es => a -> Eff es (TMVar a)
newTMVarIO = unsafeEff_ . STM.newTMVarIO

newEmptyTMVarIO :: StructuredConcurrency :> es => Eff es (TMVar a)
newEmptyTMVarIO = unsafeEff_ STM.newEmptyTMVarIO
