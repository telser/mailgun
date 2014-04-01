{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

module Rackspace.MailGun where

import           Control.Failure
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.ByteString.Char8                 as BS (ByteString, pack,
                                                              putStrLn)
import qualified Data.ByteString.Lazy.Char8            as LBS (ByteString)
import           Data.Conduit
import qualified Data.Conduit.List                     as CL
import           Data.Text                             as T (Text, concat, pack)
import           Data.Text.Encoding                    (encodeUtf8)
import           Network                               (withSocketsDo)
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Conduit

baseUrl :: String
baseUrl = "https://api.mailgun.net/v2"

data Message = TextMessage
                { from    :: Text
                , to      :: Text
                , cc      :: Maybe Text
                , bcc     :: Maybe Text
                , subject :: Maybe Text
                , text    :: Text }
             | HtmlMessage
                { from    :: Text
                , to      :: Text
                , cc      :: Maybe Text
                , bcc     :: Maybe Text
                , subject :: Maybe Text
                , html    :: Text }
             deriving (Eq, Show)

partText :: Text -> Text -> [Part]
partText name value = [ partBS name (encodeUtf8 value) ]

partMaybeText :: Text -> Maybe Text -> [Part]
partMaybeText name value = case value of
                            Just val    -> [ partBS name (encodeUtf8 val) ]
                            Nothing     -> []

buildTail :: Message -> [Part]
buildTail TextMessage{..} = partText "text" text
buildTail HtmlMessage{..} = partText "html" html

buildBase :: Message -> [Part]
buildBase msg = partText "from" (from msg)
             ++ partText "to" (to msg)
             ++ partMaybeText "cc" (cc msg)
             ++ partMaybeText "bcc" (bcc msg)
             ++ partMaybeText "subject" (subject msg)
             ++ buildTail msg

--sendMessage :: MonadIO m => String -> String -> Message -> m (Response ByteString)
--sendMessage :: (HttpException m, MonadIO m, MonadBaseControl IO m) =>
--                String -> String -> Message -> m (Response ByteString)
sendMessage :: (Failure HttpException m, MonadBaseControl IO m, MonadIO m) =>
                String -> String -> Message -> m (Response LBS.ByteString)
sendMessage domain apiKey message = do
        initReq <- parseUrl $ baseUrl ++ "/" ++ domain ++ "/messages"
        let authReq = applyBasicAuth "api" (BS.pack apiKey) initReq
            postReq = authReq { method = "POST" }
        res <- withManager $ \m -> flip httpLbs m =<<
            (formDataBody (buildBase message) postReq)
        return res
