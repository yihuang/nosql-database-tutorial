{-# Options -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric, StandaloneDeriving #-}
module Instances () where

import GHC.Generics

import Data.Text (Text)
import qualified Data.Text.Encoding as T

import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Serialize (Serialize(get, put))
import Data.Aeson (Value(..))
import Data.Attoparsec.Number (Number(..))

instance (Eq k, Hashable k, Serialize k, Serialize v) => Serialize (HashMap k v) where
    get = fmap HM.fromList get
    put = put . HM.toList

instance Serialize Text where
    get = fmap T.decodeUtf8 get
    put = put . T.encodeUtf8

instance Serialize a => Serialize (Vector a) where
    get = fmap V.fromList get
    put = put . V.toList

deriving instance Generic Number
instance Serialize Number

deriving instance Generic Value
instance Serialize Value
