{-#LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import System.Environment (getEnv)
import Twilio
import Twilio.Calls as Calls
import Twilio.Messages

main :: IO ()
main = runTwilio' (getEnv AC358a60437a112c5c59d3b52da1f0dcc7)
                  (getEnv e7ae1b711f733bae6c2647bd62154b77) $ do
  -- Print Calls.
  calls <- Calls.get
  liftIO $ print calls

  -- Send a Message.
  let body = PostMessage "+14158059869" "+14158059869" "Oh, hai"
  message <- post body
  liftIO $ print message
