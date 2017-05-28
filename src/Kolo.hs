{-# LANGUAGE OverloadedStrings #-}

module Kolo
    ( newServer
    , defaultConfig
    , serve
    ) where

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

echoListener :: CN.AppData -> IO ()
echoListener a = CN.appSource a C.$$ CN.appSink a

serve :: Server -> IO ()
serve (Server (ServerConfig host port crt key)) = do
  let c = TLS.tlsConfig host port crt key
  TLS.runTCPServerTLS c echoListener
