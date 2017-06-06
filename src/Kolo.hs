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
import qualified Control.Lens as L
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as CCo
import qualified Data.Conduit.Network as CN
import qualified Data.Conduit.Network.TLS as TLS
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import qualified Network.IRC.Conduit as IRCC
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
  , users :: [(UUID.UUID, (TC.TChan IRCC.IrcMessage, CC.ThreadId))]
  }

type Server = MV.MVar ServerState

defaultConfig :: ServerConfig
defaultConfig = ServerConfig "*" 7779 "server.crt" "server.key"

newServer :: ServerConfig -> IO Server
newServer cfg = MV.newMVar (ServerState cfg [])

addUser :: Server -> UUID.UUID -> TC.TChan IRCC.IrcMessage -> CC.ThreadId -> IO ()
addUser s uuid chan tid = do
  MV.modifyMVar_ s $ (\ss -> do
      return ss { users = (uuid, (chan, tid)) : users ss }
    )

newUserChan :: IO (TC.TChan IRCC.IrcMessage)
newUserChan = atomically $ TC.newTChan

writeUserChan :: TC.TChan IRCC.IrcMessage -> IRCC.IrcMessage -> IO ()
writeUserChan chan v = atomically $ TC.writeTChan chan v

readUserChan :: TC.TChan IRCC.IrcMessage -> IO IRCC.IrcMessage
readUserChan chan = atomically $ TC.readTChan chan

runUserThread :: TC.TChan IRCC.IrcMessage -> C.Consumer BS.ByteString IO () -> IO CC.ThreadId
runUserThread chan sink = CC.forkIO $ (chanReader chan) C.=$= IRCC.ircEncoder C.$$ sink

chanWriter :: TC.TChan IRCC.IrcMessage -> C.Consumer IRCC.IrcMessage IO ()
chanWriter chan = CCo.mapM_ (writeUserChan chan)

chanReader :: TC.TChan IRCC.IrcMessage -> C.Producer IO IRCC.IrcMessage
chanReader chan = CCo.repeatM (readUserChan chan)

extractMessage :: Either BS.ByteString IRCC.IrcEvent -> IRCC.IrcMessage
extractMessage (Left e) = IRCC.RawMsg (BS.append (C8.pack "unrecognized ") e)
extractMessage (Right e) = L.view IRCC.message e

ircEventHandler :: C.Conduit (Either BS.ByteString IRCC.IrcEvent) IO IRCC.IrcMessage
ircEventHandler = CCo.map extractMessage

listen :: C.Producer IO BS.ByteString -> TC.TChan IRCC.IrcMessage -> IO ()
listen source chan = source C.=$= IRCC.ircDecoder C.=$= ircEventHandler C.$$ (chanWriter chan)

serverWelcome :: BS.ByteString -> IRCC.IrcMessage
serverWelcome user =
  IRCC.RawMsg $
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
