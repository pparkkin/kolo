{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Kolo
    ( newServer
    , defaultConfig
    , serve
    ) where

import qualified Control.Concurrent as CC
import qualified Control.Concurrent.STM.TChan as TC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Conduit as C
import qualified Data.Conduit.Network as CN
import qualified Data.Conduit.Network.TLS as TLS
import qualified Network.Simple.TCP as NS

import Control.Monad.IO.Class ( liftIO )
import Control.Monad.STM ( atomically )

data ServerConfig = ServerConfig
  { host :: CN.HostPreference
  , port :: Int
  , cert :: FilePath
  , key :: FilePath
  }

data Server = Server
  { config :: ServerConfig
  }

defaultConfig :: ServerConfig
defaultConfig = ServerConfig "*" 7779 "server.crt" "server.key"

newServer :: ServerConfig -> Server
newServer = Server

echoConduit :: C.Conduit BS.ByteString IO BS.ByteString
echoConduit = do
  input <- C.await
  case input of
    Nothing -> return ()
    Just val -> do
      C.yield (BS.append (C8.pack "You said: ") val)
      echoConduit

newUserChan :: IO (TC.TChan BS.ByteString)
newUserChan = atomically $ TC.newTChan

writeUserChan :: TC.TChan BS.ByteString -> BS.ByteString -> IO ()
writeUserChan chan v = atomically $ TC.writeTChan chan v

runUserThread :: TC.TChan BS.ByteString -> C.Consumer BS.ByteString IO () -> IO CC.ThreadId
runUserThread chan sink = CC.forkIO $ C.connect (chanReader chan) sink

chanWriter :: TC.TChan BS.ByteString -> C.Consumer BS.ByteString IO ()
chanWriter chan = do
  maybeVal <- C.await
  case maybeVal of
    Nothing -> return ()
    Just val -> do
      liftIO (writeUserChan chan val)
      chanWriter chan

chanReader :: TC.TChan BS.ByteString -> C.Producer IO BS.ByteString
chanReader chan = do
  val <- liftIO $ atomically $ TC.readTChan chan
  C.yield val
  chanReader chan

listen :: C.Producer IO BS.ByteString -> TC.TChan BS.ByteString -> IO ()
listen source chan = C.connect source (chanWriter chan)

runListener :: CN.AppData -> IO ()
runListener a = do
    chan <- newUserChan
    _ <- runUserThread chan sink
    listen source chan
  where
    source = CN.appSource a
    sink = CN.appSink a

serve :: Server -> IO ()
serve (Server (ServerConfig host port crt key)) = do
  let c = TLS.tlsConfig host port crt key
  TLS.runTCPServerTLS c runListener
