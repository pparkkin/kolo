{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Kolo
    ( newServer
    , defaultConfig
    , serve
    ) where

import qualified Control.Concurrent as CC
import qualified Control.Concurrent.MVar as MV
import qualified Control.Concurrent.STM.TChan as TC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Conduit as C
import qualified Data.Conduit.Network as CN
import qualified Data.Conduit.Network.TLS as TLS
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import qualified Network.Simple.TCP as NS

import Control.Monad.IO.Class ( liftIO )
import Control.Monad.STM ( atomically )

data ServerConfig = ServerConfig
  { host :: CN.HostPreference
  , port :: Int
  , cert :: FilePath
  , key :: FilePath
  }

data ServerState = ServerState
  { config :: ServerConfig
  , users :: [(UUID.UUID, (TC.TChan BS.ByteString, CC.ThreadId))]
  }

type Server = MV.MVar ServerState

defaultConfig :: ServerConfig
defaultConfig = ServerConfig "*" 7779 "server.crt" "server.key"

newServer :: ServerConfig -> IO Server
newServer cfg = MV.newMVar (ServerState cfg [])

addUser :: Server -> UUID.UUID -> TC.TChan BS.ByteString -> CC.ThreadId -> IO ()
addUser s uuid chan tid = do
  MV.modifyMVar_ s $ (\ss -> do
      return ss { users = (uuid, (chan, tid)) : users ss }
    )

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

serverWelcome :: BS.ByteString -> BS.ByteString
serverWelcome user =
  foldr BS.append BS.empty
    [ C8.pack "Welcome, "
    , user
    , C8.pack "!\n"
    ]

runListener :: Server -> CN.AppData -> IO ()
runListener s a = do
    chan <- newUserChan
    tid <- runUserThread chan sink
    uuid <- UUID4.nextRandom
    addUser s uuid chan tid
    writeUserChan chan (serverWelcome (UUID.toASCIIBytes uuid))
    listen source chan
  where
    source = CN.appSource a
    sink = CN.appSink a

serve :: Server -> IO ()
serve s = do
  (ServerState (ServerConfig host port crt key) _) <- MV.readMVar s
  let c = TLS.tlsConfig host port crt key
  TLS.runTCPServerTLS c (runListener s)
