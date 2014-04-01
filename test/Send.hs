{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text          as T (Text (..), pack)
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
        res <- sendMessage domain apiKey message
        print res
