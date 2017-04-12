{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Web.Twitter.AppOnly.Api.Search
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

import Web.Twitter.AppOnly.Param
       (Method(GET), ToTwitterParam(..), TwitterHasParam, TwitterRequest,
        TwitterReturn, mkTwitterRequest)

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
