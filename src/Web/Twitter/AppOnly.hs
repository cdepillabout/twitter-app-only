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
import Data.Monoid ((<>))
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple
       (addRequestHeader, defaultRequest, getResponseBody, httpLBS,
        setRequestHost, setRequestMethod, setRequestPath, setRequestPort,
        setRequestQueryString, setRequestSecure)
import Web.Twitter.Types (SearchResult, Status)

import Web.Twitter.AppOnly.Api.Search
import Web.Twitter.AppOnly.Auth
import Web.Twitter.AppOnly.Error
import Web.Twitter.AppOnly.Param

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

methodToByteString :: Method -> ByteString
methodToByteString = B8.pack . show
