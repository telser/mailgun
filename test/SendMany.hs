{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class
import           Data.Text              as T (Text (..), pack)
import           Network.HTTP.Conduit
import           Rackspace.MailGun
import           System.Environment

main :: IO ()
main = do
        domain <- getEnv "MAILGUN_DOMAIN"
        apiKey <- getEnv "MAILGUN_SECRET"
        testAddr <- getEnv "MAILGUN_TEST_ADDRESS"

        let message = TextMessage
                    { from = T.pack ("someone@" ++ domain)
                    , to = T.pack testAddr
                    , cc = Nothing
                    , bcc = Nothing
                    , subject = Just "Test Message"
                    , text = "Hello, this is a test message!" }
        withManager $ \manager -> do
                let sendW = sendWith manager
                res1 <- sendW domain apiKey message
                res2 <- sendW domain apiKey message
                res3 <- sendW domain apiKey message
                liftIO $ print res1
                liftIO $ print res2
                liftIO $ print res3
