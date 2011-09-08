 import Network.WebSockets (shakeHands, getFrame, putFrame)
 import Network (listenOn, PortID(PortNumber), accept, withSocketsDo)
 import System.IO (Handle, hClose)
 import qualified Data.ByteString as B (append, null)
 import Data.ByteString.UTF8 (fromString) -- this is from utf8-string
 import Control.Monad (forever)
 import Control.Concurrent (forkIO)

 -- Accepts clients, spawns a single handler for each one.
 main :: IO ()
 main = withSocketsDo $ do
   socket <- listenOn (PortNumber 8088)
   putStrLn "Listening on port 8088."
   forever $ do
     (h, _, _) <- accept socket
     forkIO (talkTo h)

 -- Shakes hands with client. If no error, starts talking.
 talkTo :: Handle -> IO ()
 talkTo h = do
   request <- shakeHands h
   case request of
     Left err -> print err
     Right  _ -> do
       putFrame h (fromString "Do you read me, Lieutenant Bowie?")
       putStrLn "Shook hands, sent welcome message."
       talkLoop h

 -- Talks to the client (by echoing messages back) until EOF.
 talkLoop :: Handle -> IO ()
 talkLoop h = do
   msg <- getFrame h
   if B.null msg
      then do
        putStrLn "EOF encountered. Closing handle."
        hClose h
      else do
        putFrame h $ B.append msg (fromString ", meow.")
        talkLoop h
