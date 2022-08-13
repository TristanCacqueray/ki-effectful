module Main where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TMVar, putTMVar, readTMVar)
import Control.Exception
import Effectful
import Effectful.Ki
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
    defaultMain $
        testGroup
            "ki-effectful"
            [ testCase "`fork` works" $ do
                res <- runStructuredEff testFork
                assertEqual "match" res 42
            , testCase "`fork` propagates exceptions" $ do
                assertThrow $ runStructuredEff testThrow
            , testCase "`client` works" $ do
                res <- runStructuredEff testClient
                assertEqual "match" res (Just 42)
            , testCase "`client` cancel" $ do
                res <- runStructuredEff testClientCancel
                assertEqual "match" res Nothing
            ]

runStructuredEff :: Eff '[StructuredConcurrency, IOE] a -> IO a
runStructuredEff = runEff . runStructuredConcurrency

testFork :: StructuredConcurrency :> es => Eff es Int
testFork = scoped $ \scope -> do
    child <- fork scope $ pure 42
    atomically $ await child

testThrow :: StructuredConcurrency :> es => Eff es Int
testThrow = scoped $ \scope -> do
    fork scope $ error "oops"
    child <- fork scope $ pure 42
    atomically $ do
        awaitAll scope
        await child

testClient :: StructuredConcurrency :> es => Eff es (Maybe Int)
testClient = scoped $ \scope -> do
    hitman <- newEmptyTMVarIO
    child <- fork scope $ client hitman (pure 42)
    atomically $ await child

testClientCancel :: (IOE :> es, StructuredConcurrency :> es) => Eff es (Maybe Int)
testClientCancel = scoped $ \scope -> do
    hitman <- newEmptyTMVarIO
    child <- fork scope $ client hitman $ do
            -- liftIO $ putStrLn "running"
            liftIO (threadDelay 500000)
            pure 42
    liftIO (threadDelay 100000)
    -- liftIO $ putStrLn "stopping"
    atomically $ putTMVar hitman ()
    atomically $ await child

-- | cancellable client implementation proposed in https://github.com/awkward-squad/ki/issues/11#issuecomment-1214159154
client :: StructuredConcurrency :> es => TMVar () -> Eff es a -> Eff es (Maybe a)
client doneVar action = scoped $ \scope -> do
    thread <- fork scope action
    let waitDone = do
            () <- readTMVar doneVar
            pure Nothing

        waitThread = do
            result <- await thread
            pure (Just result)

    atomically $ waitDone <|> waitThread

assertThrow :: IO a -> Assertion
assertThrow action = do
    res <- try @SomeException action
    case res of
        Left _ -> pure ()
        Right _ -> assertFailure "Action did not throw"
