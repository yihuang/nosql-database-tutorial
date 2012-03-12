{-# OPTIONS -funbox-strict-fields #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}
import Prelude           hiding (catch)
import GHC.Generics             (Generic)
import System.IO                (hClose)
import System.Directory         (renameFile)
import System.Posix.Temp        (mkstemp)
import Control.Monad            (forever)
import Control.Monad.IO.Class   (liftIO)
import Control.Exception        (catch, SomeException)
import Control.Concurrent       (forkIO, threadDelay)

import Data.Maybe               (fromMaybe)
import Data.Monoid              (mempty)
import Data.Text                (Text)
import Data.HashMap.Strict      (HashMap)
import Data.Serialize           (Serialize, decodeLazy, encodeLazy)

import qualified Data.ByteString.Lazy       as L
import qualified Blaze.ByteString.Builder   as B
import qualified Data.HashMap.Strict        as HM
import qualified Data.Aeson                 as JSON

import Data.Conduit             ( ($$) )
import Data.Conduit.List        (consume)
import Network.HTTP.Types       (status200, status404)
import Network.Wai              (Application, Request(..), Response(..))
import Network.Wai.Handler.Warp (run)

import Instances ()
import Shared (Shared, newShared, readShared, modifyShared_)

{- | 数据结构
 -}
data AuthInfo = AuthInfo
  { uid     :: !Int
  , name    :: !Text
  } deriving (Generic)

data Session = Session
  { auth    :: !AuthInfo
  , extra   :: !(HashMap Text JSON.Value)
  } deriving (Generic)

type SessionId = Text
newtype SessionStore = SessionStore { sessionMap :: HashMap SessionId Session }
    deriving (Generic)

{- | 序列化
 -}
instance Serialize AuthInfo
instance Serialize Session
instance Serialize SessionStore

{- | JSON编/解码
 -}
instance JSON.FromJSON  AuthInfo
instance JSON.ToJSON    AuthInfo
instance JSON.FromJSON  Session
instance JSON.ToJSON    Session
instance JSON.FromJSON  SessionStore
instance JSON.ToJSON    SessionStore

emptySessionStore :: SessionStore
emptySessionStore = SessionStore HM.empty

{- | 增删改查 (v代表value、k代表key、m代表map)
 -}
insert :: SessionId -> Session -> SessionStore -> SessionStore
insert k v = SessionStore . HM.insert k v . sessionMap

delete :: SessionId -> SessionStore -> SessionStore
delete k = SessionStore . HM.delete k . sessionMap

update :: SessionId -> Session -> SessionStore -> SessionStore
update k v = SessionStore . HM.insert k v . sessionMap

query :: SessionId -> SessionStore -> Maybe Session
query k = HM.lookup k . sessionMap

{- | restful interface
 -}
app :: Shared SessionStore -> Application
app shared req = case (requestMethod req, pathInfo req) of
    ("GET", ["get", sid]) -> do
        s <- liftIO $ readShared shared
        json $ query sid s
    ("POST", ["delete", sid]) -> do
        liftIO $ modifyShared_ shared (return . delete sid)
        json ()
    ("POST", ["insert", sid]) -> do
        s <- jsonBody
        liftIO $ modifyShared_ shared (return . insert sid s)
        json ()
    ("POST", ["update", sid]) -> do
        s <- jsonBody
        liftIO $ modifyShared_ shared (return . update sid s)
        json ()
    _ -> return $ ResponseBuilder status404 [] mempty
  where
    json js = return $ ResponseBuilder status200 [("Content-Type", "application/json")] (B.fromLazyByteString $ JSON.encode js)
    jsonBody = do
        body <- fmap L.fromChunks (requestBody req $$ consume)
        case JSON.decode body of
            Just r  -> return r
            Nothing -> fail "json decode failed."

{- | load from disk
 -}
loadFromFile :: Serialize a => FilePath -> IO (Maybe a)
loadFromFile path = do
    mc <- fmap Just (L.readFile path)
            `catch` (\(e::SomeException) -> do
                        print e
                        return Nothing
                    )
    case mc of
        Nothing -> return Nothing
        Just c ->
            case decodeLazy c of
                Left err -> do putStrLn err
                               return Nothing
                Right a  -> return (Just a)

{- | save to disk atomically
 -}
dumpToFile :: Serialize a => FilePath -> a -> IO ()
dumpToFile path a = do
    (path', h) <- mkstemp "sessionXXXXXX"
    L.hPut h (encodeLazy a)
    hClose h
    renameFile path' path

{- | auto save to disk periodically.
 -}
dumpPeridic :: Shared SessionStore -> FilePath -> Int -> IO ()
dumpPeridic shared path interval =
    forever $ do
        threadDelay interval
        catch (readShared shared >>= dumpToFile path)
              (\e -> putStrLn $ "uncaught exception in dumpPeridic: "++show (e::SomeException))

{- | main
 -}
main :: IO ()
main = do
    let dbfile = "./sessionstore" 
    store <- fmap (fromMaybe emptySessionStore) $ loadFromFile dbfile
    shared <- newShared store
    _ <- forkIO $ dumpPeridic shared dbfile 5000000
    run 3000 (app shared)
