{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Twitter.AppOnly
  ( ConsumerKey(..)
  , ConsumerSecret(..)
  , Credentials(..)
  , BearerToken
  , HasBearerToken(..)
  , CredentialsException(..)
  , credentialsFromEnv
  , credentialsFromEnvEx
  , createOAuth2Creds
  , bearerTokenFromCreds
  , TwitterError
  , twitter
  , Param(..)
  , Params
  , Method(DELETE, GET, POST, PUT)
  , TwitterRequest(..)
  , ToTwitterParam(..)
  , TwitterReturn
  , mkTwitterRequest
  , (-&-)
  , SearchString(..)
  , Count(..)
  , SearchTweets
  , searchTweets
  , module Web.Twitter.Types
  ) where

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

import Web.Twitter.AppOnly.Auth
import Web.Twitter.AppOnly.Error

twitter
  :: (HasBearerToken r, FromJSON (TwitterReturn a), MonadIO m)
  => r -> TwitterRequest a -> m (Either TwitterError (TwitterReturn a))
twitter hasBearerToken TwitterRequest{method, endpoint, queryParams} = do
  let (BearerToken bearerToken) = getBearerToken hasBearerToken
      initReq =
        addRequestHeader "Authorization" ("Bearer " <> bearerToken) .
        setRequestHost "api.twitter.com" .
        setRequestMethod (methodToByteString method) .
        setRequestPath ("1.1/" <> encodeUtf8 endpoint) .
        setRequestPort 443 .
        setRequestSecure True $
        defaultRequest
      req =
        case method of
          GET -> setRequestQueryString (fmap Just <$> queryParams) initReq
  resp <- httpLBS req
  case decode $ getResponseBody resp of
    Nothing -> pure $ Left ()
    Just ret -> pure $ Right ret

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

methodToByteString :: Method -> ByteString
methodToByteString = B8.pack . show

data TwitterRequest a = TwitterRequest
  { method :: Method
  , endpoint :: Text
  , queryParams :: Params
  } deriving (Data, Eq, Read, Show, Typeable)

class ToTwitterParam param where
  toTwitterParam :: param -> [(ByteString, ByteString)] -> [(ByteString, ByteString)]

class ToTwitterParam param => TwitterHasParam request param

type family TwitterReturn a :: *

mkTwitterRequest :: Method -> Text -> Params -> TwitterRequest a
mkTwitterRequest = TwitterRequest

(-&-)
  :: TwitterHasParam request param
  => TwitterRequest request -> param -> TwitterRequest request
twitReq -&- param =
  twitReq {queryParams = toTwitterParam param (queryParams twitReq)}

newtype SearchString = SearchString
  { unSearchString :: Text
  } deriving (Data, Eq, IsString, Read, Show, Typeable)

newtype Count = Count
  { unCount :: Int
  } deriving (Data, Eq, Num, Read, Show, Typeable)

instance ToTwitterParam Count where
  toTwitterParam :: Count
                 -> [(ByteString, ByteString)]
                 -> [(ByteString, ByteString)]
  toTwitterParam (Count count) =
    (("count", B8.pack $ show count) :)

instance ToTwitterParam SearchString where
  toTwitterParam :: SearchString
                 -> [(ByteString, ByteString)]
                 -> [(ByteString, ByteString)]
  toTwitterParam (SearchString query) =
    (("q", encodeUtf8 query) :)

data SearchTweets

type instance TwitterReturn SearchTweets = SearchResult [Status]

instance TwitterHasParam SearchTweets Count

searchTweets :: SearchString -> TwitterRequest SearchTweets
searchTweets searchString =
  mkTwitterRequest GET "search/tweets.json" $ toTwitterParam searchString []
