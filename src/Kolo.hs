{-# LANGUAGE OverloadedStrings #-}

module Kolo
    ( newServer
    , defaultConfig
    , serve
    ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Conduit as C
import qualified Data.Conduit.Network as CN
import qualified Data.Conduit.Network.TLS as TLS
import qualified Network.Simple.TCP as NS

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

bufferSize :: Int
bufferSize = 4096

echoConduit :: C.Conduit BS.ByteString IO BS.ByteString
echoConduit = do
  input <- C.await
  case input of
    Nothing -> return ()
    Just val -> do
      C.yield (BS.append (C8.pack "You said: ") val)
      echoConduit

echoListener :: CN.AppData -> IO ()
echoListener a = CN.appSource a C.=$= echoConduit C.$$ CN.appSink a

serve :: Server -> IO ()
serve (Server (ServerConfig host port crt key)) = do
  let c = TLS.tlsConfig host port crt key
  TLS.runTCPServerTLS c echoListener
