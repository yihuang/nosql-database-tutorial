{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import System.Random        (randomIO)
import GHC.Generics         (Generic)

import Data.Monoid          (mempty)
import Data.Text            (Text)
import Data.HashMap.Strict  (HashMap)
import Data.Serialize       (Serialize)

import qualified Data.ByteString.Lazy   as L
import qualified Blaze.ByteString.Builder as B
import qualified Data.Text              as T
import qualified Data.HashMap.Strict    as HM
import qualified Data.Aeson             as JSON

import Control.Monad (replicateM)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, Request(..), Response(..))
import Network.Wai.Handler.Warp (run)

import Instances ()

{- | persistent data structure
 -}
data AuthInfo = AuthInfo
  { uid     :: Int
  , name    :: Text
  } deriving (Generic)

data Session = Session
  { auth    :: AuthInfo
  , extra   :: HashMap Text JSON.Value
  } deriving (Generic)

type SessionId = Text
newtype SessionStore = SessionStore { sessionMap :: HashMap SessionId Session }
    deriving (Generic)

emptySessionStore :: SessionStore
emptySessionStore = SessionStore HM.empty

mkSessionId :: IO SessionId
mkSessionId = T.pack <$> replicateM 32 randomIO

{- | serialisation
 -}
instance Serialize AuthInfo
instance Serialize Session
instance Serialize SessionStore

{- | jsonify
 -}
instance JSON.FromJSON  AuthInfo
instance JSON.ToJSON    AuthInfo
instance JSON.FromJSON  Session
instance JSON.ToJSON    Session
instance JSON.FromJSON  SessionStore
instance JSON.ToJSON    SessionStore

{- | insert/delete/update/query
 -}
new :: Session -> SessionStore -> IO (SessionStore, SessionId)
new v (SessionStore m) = do
    k <- genKey
    return (SessionStore $ HM.insert k v m, k)
  where
    -- generate session id not exists in SessionStore.
    genKey = do
        k <- mkSessionId
        case HM.lookup k m of
            Nothing -> return k
            Just _  -> genKey

delete :: SessionId -> SessionStore -> SessionStore
delete k = SessionStore . HM.delete k . sessionMap

update :: SessionId -> Session -> SessionStore -> SessionStore
update k v = SessionStore . HM.insert k v . sessionMap

query :: SessionId -> SessionStore -> Maybe Session
query k = HM.lookup k . sessionMap

{- | share data type among concurrently threads.
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

{- | HTTP interface
 -}
app :: Shared SessionStore -> Application
app store req = case (requestMethod req, pathInfo req) of
    ("GET", ["get", sid]) -> do
        s <- liftIO $ readShared store
        json $ query sid s
    ("POST", ["delete", sid]) -> do
        liftIO $ modifyShared_ store (return . delete sid)
        json ()
    ("POST", ["new"]) -> do
        s <- jsonBody
        sid <- liftIO $ modifyShared store (new s)
        json sid
    ("POST", ["update", sid]) -> do
        s <- jsonBody
        liftIO $ modifyShared_ store (return . update sid s)
        json ()
    _ -> return $ ResponseBuilder status404 [] mempty
  where
    json js = return $ ResponseBuilder status200 [("Content-Type", "application/json")] (B.fromLazyByteString $ JSON.encode js)
    jsonBody = do
        body <- L.fromChunks <$> (requestBody req C.$$ C.consume)
        case JSON.decode body of
            Just r  -> return r
            Nothing -> fail "json decode failed."

{- | main
 -}
main :: IO ()
main = do
    store <- newShared emptySessionStore
    run 3000 (app store)
