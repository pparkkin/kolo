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
import qualified System.Log.Logger as Logger

import Control.Monad.IO.Class ( liftIO )
import Control.Monad.STM ( atomically )
import Data.List ( intercalate )
import System.Log.Logger ( debugM
                         , infoM
                         , warningM
                         , errorM
                         )

data ServerConfig = ServerConfig
  { host :: CN.HostPreference
  , port :: Int
  , cert :: FilePath
  , key :: FilePath
  , logLevel :: Logger.Priority
  }

data ServerState = ServerState
  { config :: ServerConfig
  , users :: [(UUID.UUID, (TC.TChan IRCC.IrcMessage, CC.ThreadId))]
  }

type Server = MV.MVar ServerState

serverLogger :: String
serverLogger = "server"

defaultConfig :: ServerConfig
defaultConfig = ServerConfig "*" 7779 "server.crt" "server.key" Logger.DEBUG

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

logMessage :: Either BS.ByteString IRCC.IrcEvent -> IO ()
logMessage (Left e) = debugM serverLogger ("Unable to decode " ++ C8.unpack e)
logMessage (Right e) = debugM serverLogger ("Got " ++ (show e))

logByteString :: String -> BS.ByteString -> IO ()
logByteString msg bs = debugM serverLogger (msg ++ C8.unpack bs)

ircEventHandler :: C.Consumer (Either BS.ByteString IRCC.IrcEvent) IO ()
ircEventHandler = CCo.mapM_ logMessage

sourcePreprocessor :: C.Conduit BS.ByteString IO BS.ByteString
sourcePreprocessor = CCo.concatMap C8.lines

sourceLogger :: C.Conduit BS.ByteString IO BS.ByteString
sourceLogger = CCo.iterM (logByteString "Received data\n")

listen :: C.Producer IO BS.ByteString -> TC.TChan IRCC.IrcMessage -> IO ()
listen source chan =
  source
    C.=$= sourceLogger
--    C.=$= sourcePreprocessor
    C.=$= IRCC.ircDecoder
    C.$$ ircEventHandler

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
    infoM serverLogger ("New connection from " ++ (show (CN.appSockAddr a)))
    uuid <- UUID4.nextRandom
    chan <- newUserChan
    tid <- runUserThread chan sink
    addUser s uuid chan tid
    writeUserChan chan (serverWelcome (UUID.toASCIIBytes uuid))
    listen source chan
  where
    source = CN.appSource a
    sink = CN.appSink a

configureLogger :: Logger.Priority -> IO ()
configureLogger logLevel = do
  Logger.updateGlobalLogger serverLogger (Logger.setLevel logLevel)

serve :: Server -> IO ()
serve s = do
  (ServerState (ServerConfig host port crt key logLevel) _) <- MV.readMVar s
  let c = TLS.tlsConfig host port crt key
  configureLogger logLevel
  infoM serverLogger ("Listening on port " ++ (show port))
  TLS.runTCPServerTLS c (runListener s)
