module Effectful.Ki (
    -- * Effect
    StructuredConcurrency,

    -- * Handlers
    runStructuredConcurrency,
    withCurrentScope,

    -- * Core API
    Scope,
    Thread,
    fork,
    forkTry,
    await,
    awaitAll,
    withAwaitAll,

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
import Effectful.Dispatch.Static.Primitive (cloneEnv)

import Ki hiding (fork, forkTry, fork_, scoped)
import Ki qualified

data StructuredConcurrency :: Effect

type instance DispatchOf StructuredConcurrency = 'Static 'WithSideEffects
data instance StaticRep StructuredConcurrency = StructuredConcurrency Scope

-- | Run the 'StructuredConcurrency' effect.
runStructuredConcurrency :: IOE :> es => Eff (StructuredConcurrency : es) a -> Eff es a
runStructuredConcurrency k = withEffToIO $ \runInIO ->
    Ki.scoped $ \scope ->
        runInIO $ evalStaticRep (StructuredConcurrency scope) k

-- | Provide a callback function to run an action within the current `Scope`.
withCurrentScope ::
    StructuredConcurrency :> es =>
    ((forall es' a. IOE :> es' => Eff (StructuredConcurrency : es') a -> Eff es' a) -> Eff es b) ->
    Eff es b
withCurrentScope f = do
    rep <- getStaticRep
    f (evalStaticRep rep)

fork ::
    StructuredConcurrency :> es =>
    Eff es a ->
    Eff es (Thread a)
fork action = do
    StructuredConcurrency scope <- getStaticRep
    unsafeEff $ \es -> do
        es' <- cloneEnv es
        Ki.fork scope (unEff action es')

forkTry ::
    Exception e =>
    StructuredConcurrency :> es =>
    Eff es a ->
    Eff es (Thread (Either e a))
forkTry action = do
    StructuredConcurrency scope <- getStaticRep
    unsafeEff $ \es -> do
        es' <- cloneEnv es
        Ki.forkTry scope (unEff action es')

fork_ ::
    StructuredConcurrency :> es =>
    Eff es Void ->
    Eff es ()
fork_ action = do
    StructuredConcurrency scope <- getStaticRep
    unsafeEff $ \es -> do
        es' <- cloneEnv es
        Ki.fork_ scope (unEff action es')

withAwaitAll :: StructuredConcurrency :> es => (STM () -> STM a) -> Eff es a
withAwaitAll f = do
    StructuredConcurrency scope <- getStaticRep
    unsafeEff_ $ STM.atomically $ f $ Ki.awaitAll scope

atomically :: StructuredConcurrency :> es => STM a -> Eff es a
atomically = unsafeEff_ . STM.atomically

newTVarIO :: StructuredConcurrency :> es => a -> Eff es (TVar a)
newTVarIO = unsafeEff_ . STM.newTVarIO

newTMVarIO :: StructuredConcurrency :> es => a -> Eff es (TMVar a)
newTMVarIO = unsafeEff_ . STM.newTMVarIO

newEmptyTMVarIO :: StructuredConcurrency :> es => Eff es (TMVar a)
newEmptyTMVarIO = unsafeEff_ STM.newEmptyTMVarIO
