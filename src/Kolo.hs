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
import qualified Network.IRC as I
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
  , users :: [(UUID.UUID, (TC.TChan I.Message, CC.ThreadId))]
  }

type Server = MV.MVar ServerState

serverLogger :: String
serverLogger = "server"

defaultConfig :: ServerConfig
defaultConfig = ServerConfig "*" 7779 "server.crt" "server.key" Logger.DEBUG

newServer :: ServerConfig -> IO Server
newServer cfg = MV.newMVar (ServerState cfg [])

addUser :: Server -> UUID.UUID -> TC.TChan I.Message -> CC.ThreadId -> IO ()
addUser s uuid chan tid = do
  MV.modifyMVar_ s $ (\ss -> do
      return ss { users = (uuid, (chan, tid)) : users ss }
    )

newUserChan :: IO (TC.TChan I.Message)
newUserChan = atomically $ TC.newTChan

writeUserChan :: TC.TChan I.Message -> I.Message -> IO ()
writeUserChan chan v = atomically $ TC.writeTChan chan v

readUserChan :: TC.TChan I.Message -> IO I.Message
readUserChan chan = atomically $ TC.readTChan chan

runUserThread :: TC.TChan I.Message -> C.Consumer BS.ByteString IO () -> IO CC.ThreadId
runUserThread chan sink = CC.forkIO $ (chanReader chan) C.=$= IRCC.ircEncoderMessage C.$$ sink

chanWriter :: TC.TChan I.Message -> C.Consumer I.Message IO ()
chanWriter chan = CCo.mapM_ (writeUserChan chan)

chanReader :: TC.TChan I.Message -> C.Producer IO I.Message
chanReader chan = CCo.repeatM (readUserChan chan)

logMessage :: Either BS.ByteString I.Message -> IO ()
logMessage (Left e) = debugM serverLogger ("Unable to decode " ++ C8.unpack e)
logMessage (Right e) = debugM serverLogger ("Got " ++ (show e))

logByteString :: String -> BS.ByteString -> IO ()
logByteString msg bs = debugM serverLogger (msg ++ C8.unpack bs)

-- remove newlines at the end of input data
sanitizeData :: BS.ByteString -> BS.ByteString
sanitizeData = head . C8.lines

handleMessage' :: Either BS.ByteString I.Message -> IO ()
handleMessage' (Left _) = debugM serverLogger "Ignoring raw byte string"
handleMessage' (Right m) = handleMessage m

handleMessage :: I.Message -> IO ()
handleMessage (I.Message _ "CAP" _) = debugM serverLogger "IRCv3 client capability negotiation not supported"
handleMessage m = debugM serverLogger ("Unsupported message " ++ (show m))

ircEventHandler :: C.Consumer (Either BS.ByteString I.Message) IO ()
ircEventHandler = CCo.mapM_ handleMessage'

sourcePreprocessor :: C.Conduit BS.ByteString IO BS.ByteString
sourcePreprocessor = CCo.map sanitizeData

sourceLogger :: String -> C.Conduit BS.ByteString IO BS.ByteString
sourceLogger s = CCo.iterM (logByteString s)

ircMessageLogger :: C.Conduit (Either BS.ByteString I.Message) IO (Either BS.ByteString I.Message)
ircMessageLogger = CCo.iterM logMessage

listen :: C.Producer IO BS.ByteString -> TC.TChan I.Message -> IO ()
listen source chan =
  source
--    C.=$= sourceLogger "Received data\n"
--    C.=$= sourcePreprocessor
--    C.=$= sourceLogger "Processed data\n"
    C.=$= IRCC.ircDecoderMessage
    C.=$= ircMessageLogger
    C.$$ ircEventHandler


runListener :: Server -> CN.AppData -> IO ()
runListener s a = do
    infoM serverLogger ("New connection from " ++ (show (CN.appSockAddr a)))
    uuid <- UUID4.nextRandom
    chan <- newUserChan
    tid <- runUserThread chan sink
    addUser s uuid chan tid
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
