{-# LANGUAGE OverloadedStrings #-}

module Socket(
  initServer,
  initClient,
  encodeBS,
  transmitChanges,
  readLineSocket,
  sendLineSocket
  ) where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, send)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import System.IO
import Control.Exception

initServer :: Int -> IO Socket
initServer port = withSocketsDo $ do
  addr <- resolve port
  sock <- open addr
  listen sock 5
  putStrLn $ "Server started on port " ++ show port
  putStrLn   "Waiting for other player to connect...."
  (clientSock, sockAddr) <- accept sock
  return clientSock


resolve :: Int -> IO AddrInfo
resolve port = do
    let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) Nothing (Just $ show port)
    return addr

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    return sock


initClient :: Int -> IO Socket
initClient port = withSocketsDo $ do
  addrInfo <- getAddrInfo Nothing (Just "127.0.0.1") (Just $ show port)
  let serverAddr = head addrInfo
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  catch(connect sock (addrAddress serverAddr))
       (\e -> putStrLn $ "Error: " ++ show (e :: SomeException))
  return sock

encodeBS :: String -> BS.ByteString
encodeBS = TE.encodeUtf8 . T.pack

transmitChanges :: (Int, Int) -> Socket -> IO Int
transmitChanges changes socket = do
  let change = encodeBS $ show changes
  send socket change

readLineSocket :: Socket -> IO String
readLineSocket sock = do
  line <- recv sock 100
  return $ show line

sendLineSocket :: Socket -> String -> IO Int
sendLineSocket sock text = do
  send sock (encodeBS text)
