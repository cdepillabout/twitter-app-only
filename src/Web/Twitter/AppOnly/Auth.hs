{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Web.Twitter.AppOnly.Auth
  ( ConsumerKey(..)
  , ConsumerSecret(..)
  , Credentials(..)
  , BearerToken(..)
  , HasBearerToken(..)
  , CredentialsException(..)
  , credentialsFromEnv
  , credentialsFromEnvEx
  , createOAuth2Creds
  , bearerTokenFromCreds
  , TwitterError
  ) where

import Control.FromSum (fromMaybeM)
import Control.Exception (Exception(displayException))
import Control.Monad.Catch (MonadCatch, MonadThrow(throwM), handle)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON(..), Value, (.:), decode, withObject)
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Base64 (encode)
import Data.Data (Data)
import Data.Monoid ((<>))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import Network.HTTP.Simple
       (HttpException, Request, Response, addRequestHeader,
        defaultRequest, getResponseBody, httpLBS, setRequestBodyLBS,
        setRequestHost, setRequestMethod, setRequestPath, setRequestPort,
        setRequestSecure)
import Network.HTTP.Types.URI (urlEncode)
import System.ReadEnvVar (lookupEnv)

import Web.Twitter.AppOnly.Error (TwitterError)

newtype ConsumerKey = ConsumerKey
  { unConsumerKey :: ByteString
  } deriving (Data, Eq, IsString, Ord, Read, Show, Typeable)

newtype ConsumerSecret = ConsumerSecret
  { unConsumerSecret :: ByteString
  } deriving (Data, Eq, IsString, Ord, Read, Show, Typeable)

newtype Credentials = Credentials
  { unCredentials :: ByteString
  } deriving (Data, Eq, IsString, Ord, Read, Show, Typeable)

newtype BearerToken = BearerToken
  { unBearerToken :: ByteString
  } deriving (Data, Eq, IsString, Ord, Read, Show, Typeable)

instance FromJSON BearerToken where
  parseJSON :: Value -> Parser BearerToken
  parseJSON = withObject "BearerToken" $ \obj -> do
    tokenType <- obj .: "token_type" :: Parser Text
    token <- obj .: "access_token"
    case tokenType of
      "bearer" -> pure . BearerToken $ encodeUtf8 token
      _ -> fail "BearerToken's \"token_type\" is not \"bearer\"."

class HasBearerToken r where
  getBearerToken :: r -> BearerToken

instance HasBearerToken BearerToken where
  getBearerToken :: BearerToken -> BearerToken
  getBearerToken = id

data CredentialsException = CredentialsNotEnvVar
  deriving (Data, Eq, Read, Show, Typeable)

instance Exception CredentialsException where
  displayException :: CredentialsException -> String
  displayException CredentialsNotEnvVar =
    "The expected twitter credentials could not be read from the\n" <>
    "environment variables \"TWITTER_CONSUMER_KEY\" and\n" <>
    "\"TWITTER_CONSUMER_SECRET\".  Do you need to set these\n" <>
    "environment variables?"

credentialsFromEnv :: MonadIO m => m (Maybe Credentials)
credentialsFromEnv = do
  consumerKey <- lookupEnv "TWITTER_CONSUMER_KEY"
  consumerSecret <- lookupEnv "TWITTER_CONSUMER_SECRET"
  pure $ createOAuth2Creds <$> consumerKey <*> consumerSecret

credentialsFromEnvEx :: (MonadThrow m, MonadIO m) => m Credentials
credentialsFromEnvEx =
  fromMaybeM (throwM CredentialsNotEnvVar) =<< credentialsFromEnv

createOAuth2Creds :: ConsumerKey -> ConsumerSecret -> Credentials
createOAuth2Creds (ConsumerKey consumerKey) (ConsumerSecret consumerSecret) =
  let urlEncodedConsumerKey = urlEncode False consumerKey
      urlEncodedConsumerSecret = urlEncode False consumerSecret
      credentials = urlEncodedConsumerKey <> ":" <> urlEncodedConsumerSecret
      b64Credentials = encode credentials
  in Credentials b64Credentials

createOAuth2TokenReq :: Credentials -> Request
createOAuth2TokenReq (Credentials credentials) =
  addRequestHeader "Authorization" ("Basic " <> credentials) .
  addRequestHeader
    "Content-Type"
    "application/x-www-form-urlencoded;charset=UTF-8" .
  setRequestBodyLBS "grant_type=client_credentials" .
  setRequestHost "api.twitter.com" .
  setRequestMethod "POST" .
  setRequestPath "oauth2/token" .
  setRequestPort 443 .
  setRequestSecure True $
  defaultRequest

respToBearerToken :: Response LBS.ByteString -> Either TwitterError BearerToken
respToBearerToken resp =
  let body = getResponseBody resp
  in case decode body of
       Nothing -> Left ()
       Just ret -> Right ret

bearerTokenFromCreds
  :: forall m.
     (MonadCatch m, MonadIO m)
  => Credentials -> m (Either TwitterError BearerToken)
bearerTokenFromCreds creds = do
  let req = createOAuth2TokenReq creds
  handle err $ respToBearerToken <$> httpLBS req
  where
    err :: HttpException -> m (Either TwitterError BearerToken)
    err _ = pure $ Left ()
