{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent     (forkIO, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay)
import           Control.Exception      (SomeException, bracket, finally, throwIO, try)
import           Control.Monad          (replicateM_, unless)
import           Control.Monad.Except   (ExceptT, runExceptT, throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, runReaderT)

import           Data.IORef             (modifyIORef', newIORef, readIORef)
import           Data.Pool              (createPool, destroyAllResources)
import qualified Data.Text              as Text

import           System.Timeout         (timeout)

import qualified VVA.API                as API
import           VVA.API.Types          (HexText (..))
import           VVA.Pool               (ConnectionPool, withPool)
import           VVA.Types              (AppError (ValidationError))

within :: IO a -> IO a
within action = timeout 1000000 action >>= maybe (fail "Pool capacity leaked") pure

run :: ConnectionPool -> ExceptT String (ReaderT ConnectionPool IO) a -> IO (Either String a)
run pool action = runReaderT (runExceptT action) pool

main :: IO ()
main = do
  created <- newIORef (0 :: Int)
  destroyed <- newIORef (0 :: Int)
  -- Only resource ownership is exercised: no SQL operation may force this sentinel.
  let acquire = modifyIORef' created (+ 1) >> pure (error "Unexpected database use in pool test")
      release _ = modifyIORef' destroyed (+ 1)
  bracket (createPool acquire release 1 60 1) destroyAllResources $ \pool -> do
    result <- within $ run pool $ withPool $ \_ -> pure (42 :: Int)
    unless (result == Right 42) $ fail "Successful result changed"
    replicateM_ 3 $ do
      missing <- within $ run pool $ withPool $ \_ -> throwError "not found"
      unless (missing == (Left "not found" :: Either String ())) $ fail "Application error changed"
    count <- readIORef created
    unless (count == 1) $ fail "Application errors should return the resource"
    failed <- try (within $ run pool $ withPool $ \_ -> liftIO $ throwIO $ userError "SQL failure")
      :: IO (Either SomeException (Either String ()))
    case failed of
      Left _  -> pure ()
      Right _ -> fail "IO exception was swallowed"
    closed <- readIORef destroyed
    unless (closed == 1) $ fail "IO failure should destroy the resource"
    entered <- newEmptyMVar
    finished <- newEmptyMVar
    worker <- forkIO $ (do
      _ <- try (run pool $ withPool $ \_ -> liftIO $ putMVar entered () >> threadDelay 10000000)
        :: IO (Either SomeException (Either String ()))
      pure ()) `finally` putMVar finished ()
    within $ takeMVar entered
    killThread worker
    within $ takeMVar finished
    cancelled <- readIORef destroyed
    unless (cancelled == 2) $ fail "Cancellation should destroy the resource"
    recovered <- within $ run pool $ withPool $ \_ -> pure ()
    unless (recovered == Right ()) $ fail "Pool did not recover"
  invalid <- within $ runReaderT
    (runExceptT $ API.getSurveyDefinition (HexText $ Text.replicate 32 "ab") 65536)
    (error "Invalid survey index must not access the application environment")
  case invalid of
    Left (ValidationError _) -> pure ()
    _                        -> fail "Out-of-range survey index was not rejected"
  putStrLn "Pool success, application error, IO exception and cancellation checks passed"
