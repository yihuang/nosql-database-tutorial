{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
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

import Control.Monad.IO.Class (liftIO)
import Data.Conduit ( ($$) )
import Data.Conduit.List (consume)
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, Request(..), Response(..))
import Network.Wai.Handler.Warp (run)

import Instances ()
import Shared (Shared(..), newShared, readShared, modifyShared_, modifyShared)

{- | 数据结构
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
app store req = case (requestMethod req, pathInfo req) of
    ("GET", ["get", sid]) -> do
        s <- liftIO $ readShared store
        json $ query sid s
    ("POST", ["delete", sid]) -> do
        liftIO $ modifyShared_ store (return . delete sid)
        json ()
    ("POST", ["insert", sid]) -> do
        s <- jsonBody
        liftIO $ modifyShared_ store (return . insert sid s)
        json ()
    ("POST", ["update", sid]) -> do
        s <- jsonBody
        liftIO $ modifyShared_ store (return . update sid s)
        json ()
    _ -> return $ ResponseBuilder status404 [] mempty
  where
    json js = return $ ResponseBuilder status200 [("Content-Type", "application/json")] (B.fromLazyByteString $ JSON.encode js)
    jsonBody = do
        body <- fmap L.fromChunks (requestBody req $$ consume)
        case JSON.decode body of
            Just r  -> return r
            Nothing -> fail "json decode failed."

{- | main
 -}
main :: IO ()
main = do
    store <- newShared emptySessionStore
    run 3000 (app store)
