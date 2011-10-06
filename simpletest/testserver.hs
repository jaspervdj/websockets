
{-# LANGUAGE OverloadedStrings #-}

import Network.WebSockets
import Network.WebSockets.Types
import qualified Network.WebSockets.Socket as WSS

import qualified Data.Text.Lazy as TL

import Control.Monad.Trans
import Data.Maybe

import Data.Enumerator

import qualified Data.ByteString as B

myApp :: Request -> WebSockets ()
myApp req = do
    liftIO . putStrLn $ "Got Request"
    sendResponse $ requestResponse req
    liftIO . putStrLn $ "Send Response"
    sendTextData $ TL.pack "Hello World"
    liftIO . putStrLn $ "Sent Hello World"
    t <- fromMaybe (error "Nothing, that's bad") `fmap` receiveData
    -- again, why is this maybe?
    liftIO . putStrLn $ "Received Data: "++TL.unpack t
    sendTextData t
    liftIO . putStrLn $ "Sent Data Back"
    liftIO . putStrLn $ "Exit"

main :: IO ()
main = WSS.runServer "0.0.0.0" 8001 myApp
{-main = print =<< (run $ enumList 1 [cld] $$
    runWebSocketsHandshake myApp putter)-}

-- The sample from the -00 spec.
cld :: B.ByteString
cld =
       "GET /demo HTTP/1.1\r\n\
       \Host: example.com\r\n\
       \Connection: Upgrade\r\n\
       \Sec-WebSocket-Key2: 12998 5 Y3 1  .P00\r\n\
       \Sec-WebSocket-Protocol: sample\r\n\
       \Upgrade: WebSocket\r\n\
       \Sec-WebSocket-Key1: 4 @1  46546xW%0l 1 5\r\n\
       \Origin: http://example.com\r\n\r\n\
       \^n:ds[4U"

putter :: Iteratee B.ByteString IO ()
putter = printChunks True
{-putter = continue $ \s ->
    case s of
        Chunks cs -> do
            liftIO $ putStrLn "-----"
            liftIO $ print cs
            liftIO $ putStrLn "-----"
            putter
        EOF -> do
            liftIO $ putStrLn "--- EOF ---"
            return ()-}

req :: RequestHttpPart
req = RequestHttpPart
    { requestHttpPath = "/test"
    , requestHttpHeaders = []
    }

foo :: IO ()
foo = do
    i <- runIteratee $ runWebSockets req myApp putter
    case i of
        Error err -> do
            putStrLn $ "---- error: "++show err
        Yield _ _ -> putStrLn "---- done."
        Continue f -> do
            putStrLn "---- continue. (possibly ok with EOF)"
            j <- runIteratee $ f EOF
            case j of
                Error err -> do
                    putStrLn $ "---- error: "++show err
                Yield x rem -> putStrLn $ "---- done: "++show x++"\n, "++show rem
                Continue f -> do
                    putStrLn "---- divergent (i.e. Continue after EOF)."

