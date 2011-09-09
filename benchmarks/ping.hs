{-# LANGUAGE OverloadedStrings #-}
import Network.WebSockets (shakeHands, getFrame, putFrame)
import Network (listenOn, PortID(PortNumber), accept, withSocketsDo)
import System.IO (Handle, hClose)
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (fromString) -- this is from utf8-string
import qualified Data.ByteString.Char8 as BC
import Control.Monad (forever)
import Control.Concurrent (forkIO)

main :: IO ()
main = withSocketsDo $ do
    socket <- listenOn (PortNumber 8088)
    putStrLn "Listening on port 8088."
    forever $ do
        (h, _, _) <- accept socket
        forkIO (ping h)

ping :: Handle -> IO ()
ping h = do
    request <- shakeHands h
    case request of
        Left err -> print err
        Right  _ -> do
            putFrame h (fromString "Ping 0")
            ping'
  where
    ping' = do
        msg <- getFrame h
        if "Pong " `B.isPrefixOf` msg
            then do
                let n = read (BC.unpack (B.drop 5 msg)) :: Int
                putFrame h (fromString $ "Ping " ++ show (n + 1))
                ping'
            else do
                putStrLn "Closed."
                hClose h
