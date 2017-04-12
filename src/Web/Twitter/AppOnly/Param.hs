{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Twitter.AppOnly.Param
  where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON(..), decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Data (Data)
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import Network.HTTP.Simple
       (addRequestHeader, defaultRequest, getResponseBody, httpLBS,
        setRequestHost, setRequestMethod, setRequestPath, setRequestPort,
        setRequestQueryString, setRequestSecure)
import Web.Twitter.Types (SearchResult, Status)

newtype Param k v = Param
  { unParam :: (k, v)
  } deriving (Data, Eq, Read, Show, Typeable)

type Params = [(ByteString, ByteString)]

data Method
  = DELETE
  | GET
  | POST
  | PUT
  deriving (Data, Eq, Read, Show, Typeable)

data TwitterRequest a = TwitterRequest
  { method :: Method
  , endpoint :: Text
  , queryParams :: Params
  } deriving (Data, Eq, Read, Show, Typeable)

class ToTwitterParam param where
  toTwitterParam :: param -> [(ByteString, ByteString)] -> [(ByteString, ByteString)]

class ToTwitterParam param => TwitterHasParam request param

type family TwitterReturn a :: *

-- | Smart constructor for 'TwitterRequest'.
mkTwitterRequest :: Method -> Text -> Params -> TwitterRequest a
mkTwitterRequest = TwitterRequest

(-&-)
  :: TwitterHasParam request param
  => TwitterRequest request -> param -> TwitterRequest request
twitReq -&- param =
  twitReq {queryParams = toTwitterParam param (queryParams twitReq)}
