module Shared
  ( Shared(..)
  , newShared
  , readShared
  , modifyShared
  , modifyShared_
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

{- | 为haskell数据类型提供线程安全的包装，写操作序列化、读操作lock-free。
 -}
data Shared a = Shared
  { sharedLock  :: MVar ()
  , sharedState :: IORef a
  }

newShared :: a -> IO (Shared a)
newShared a = Shared <$> newMVar () <*> newIORef a

readShared :: Shared a -> IO a
readShared = readIORef . sharedState

modifyShared :: Shared a -> (a -> IO (a, b)) -> IO b
modifyShared (Shared lock state) f =
    withMVar lock $ \_ -> do
        st <- readIORef state
        (st', r) <- f st
        writeIORef state st'
        return r

modifyShared_ :: Shared a -> (a -> IO a) -> IO ()
modifyShared_ ss f =
    modifyShared ss $ \s -> do
        s' <- f s
        return (s', ())

