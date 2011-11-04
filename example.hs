-- | This is the example included in the 'Network.WebSockets' documentation
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Trans (liftIO)
import qualified Data.Text as T

import Network.WebSockets
import Control.Monad (forever)

-- Talks to the client (by echoing messages back) until EOF.
talkLoop :: TextProtocol p => WebSockets p ()
talkLoop = forever $ do
    msg <- receiveData
    sendTextData $ msg `T.append` ", meow."
