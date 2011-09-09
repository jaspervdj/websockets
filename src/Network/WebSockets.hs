{- | How do you use this library? Here's how:

 * Get a 'Handle' to your connected client.

 * Perform the initial handshake with 'shakeHands' (or 'getRequest' and 'putResponse').

 * Send and receive strict bytestrings with 'putFrame' and 'getFrame'.

And here's a short example of a server that accepts clients, greets
them with a welcome message, checks for disconnects and replies to all
messages by echoing them back with an appended meow:

> import Network.WebSockets (shakeHands, getFrame, putFrame)
> import Network (listenOn, PortID(PortNumber), accept, withSocketsDo)
> import System.IO (Handle, hClose)
> import qualified Data.ByteString as B (append, null)
> import Data.ByteString.UTF8 (fromString) -- this is from utf8-string
> import Control.Monad (forever)
> import Control.Concurrent (forkIO)
>
> -- Accepts clients, spawns a single handler for each one.
> main :: IO ()
> main = withSocketsDo $ do
>   socket <- listenOn (PortNumber 8088)
>   putStrLn "Listening on port 8088."
>   forever $ do
>     (h, _, _) <- accept socket
>     forkIO (talkTo h)
>
> -- Shakes hands with client. If no error, starts talking.
> talkTo :: Handle -> IO ()
> talkTo h = do
>   request <- shakeHands h
>   case request of
>     Left err -> print err
>     Right  _ -> do
>       putFrame h (fromString "Do you read me, Lieutenant Bowie?")
>       putStrLn "Shook hands, sent welcome message."
>       talkLoop h
>
> -- Talks to the client (by echoing messages back) until EOF.
> talkLoop :: Handle -> IO ()
> talkLoop h = do
>   msg <- getFrame h
>   if B.null msg
>      then do
>        putStrLn "EOF encountered. Closing handle."
>        hClose h
>      else do
>        putFrame h $ B.append msg (fromString ", meow.")
>        talkLoop h

The example above will suffice if you wish to accept any
WebSocket-capable client, regardless of its origin or target. It won't
suffice if you have to filter the incoming clients by the contents of
their requests. For that, you can use 'getRequest' and 'putResponse',
which allow you to inspect the request details /before/ you send back
a response, if any.

If you have any suggestions, bug reports and\/or fixes, feel free to
send them to <mailto:sinisa@bidin.cc>. Thanks! -}
module Network.WebSockets (shakeHands) where

import qualified Network.WebSockets.Handshake as H
import qualified Network.WebSockets.Decode as D
import qualified Network.WebSockets.Encode as E
import qualified Network.WebSockets.WebSocket as I

-- | Accept and perform a handshake, no matter the request contents.
--
-- As long as the request is well-formed, the client will receive a
-- response saying, essentially, \"proceed\". Use this function if you
-- don't care who you're connected to, as long as that someone speaks
-- the WebSocket protocol.
--
-- The function returns either a 'HandshakeError' in case of error, or
-- a 'Request' on success. The 'Request' is returned purely for
-- logging purposes, since the handshake has already been
-- executed. Use this function immediately after establishing the
-- connection.
--
-- If you wish not to blindly accept requests but to filter them
-- according to their contents, use the 'getRequest' and 'putResponse'
-- functions.
shakeHands :: I.WebSocket -> IO (Either H.HandshakeError ())
shakeHands ws = do
  request <- I.receive D.request ws
  case request of
      Just r  -> case H.handshake r of
           Left err -> return $ Left err 
           Right r' -> I.send E.response ws r' >> return (Right ())
      Nothing -> return $ Left $ H.HandshakeError "Couldn't parse request"
