{-# LANGUAGE CPP #-}

-- | A utility for doing live display of tcp connections in the browser.
-- | Started out as a copy of the Haste.App chatbox demo program.

import Haste.App
import Haste.App.Concurrent
import qualified Control.Concurrent as C
import Control.Monad
import Control.Applicative
import Data.IORef
import qualified Data.Set as S

import Foreign
import Network.Pcap
import qualified Network.Info as N
#ifndef __HASTE__
import TcpPacket
import LocateIP
#endif

-- A server to client message:
data Message = Message String Double Double deriving Show

-- Message must be instance of Binary for server -> client communication.
instance Binary Message where
  put (Message txt long lat) = put txt >> put long >> put lat
  get = do
      tx <- get
      lo <- get
      la <- get
      return $ Message tx lo la

-- | The type representing our state - a list matching active clients with
--   the MVars used to notify them of a new message, and a backlog of messages.
type State = IORef [(SessionID, C.MVar Message)]

-- | Data type to hold all our API calls
data API = API {
    apiHello :: Remote (Server ()),
    apiAwait :: Remote (Server Message)
  }

-- | Tell the server we're here and remove any stale sessions.
hello :: Server State -> Server ()
hello state = do
  sid <- getSessionID
  active <- getActiveSessions
  clients <- state
  liftIO $ do
    v <- C.newEmptyMVar
    atomicModifyIORef clients $ \cs ->
      ((sid, v) : filter (\(sess, _) -> sess `S.member` active) cs, ())
    return ()

-- | Send a message by notifying everyone who is waiting for a message.
send :: Server State -> Message -> Server ()
send state msg = do
  clients <- state
  liftIO $ do
    cs <- readIORef clients
    -- Fork a new thread for each MVar so slow clients don't hold up fast ones.
    forM_ cs $ \(_, v) -> C.forkIO $ C.putMVar v msg

-- | Block until a new message arrives, then return it.
await :: Server State -> Server Message
await state = do
  sid <- getSessionID
  clients <- state
  liftIO $ readIORef clients >>= maybe (return $ Message ""  0.0  0.0) C.takeMVar . lookup sid

#ifndef __HASTE__
process :: Server State -> PcapHandle -> [N.IPv4] -> (N.IPv4 -> IO IPLookupResults) -> Server ()
process state handle localIPv4 getIPLocation' = do
    (hdr,pkt) <- liftIO $ Network.Pcap.next handle
    bytes <- liftIO $ peekArray (fromIntegral (hdrCaptureLength hdr)) pkt
    case filterEthernet bytes of 
        Just packet@(Packet sa _ da _ _) -> do
            let remoteIp = if sa `elem` localIPv4 then da else sa 
            lookupResults  <- liftIO $ getIPLocation' remoteIp
            let maybeLoc = location lookupResults
            case maybeLoc of
                Just loc -> send state $ Message (show packet) (latitude loc) (longitude loc)
                Nothing -> return ()
            process state handle localIPv4 (lookupFunction lookupResults) -- loop forever

        _ -> process state handle localIPv4 getIPLocation' -- loop forever

sniff :: Server State -> Server ()
sniff state = do
  handle <- liftIO $ openLive "wlan0" 500 False 0
  localInterfaces <- liftIO N.getNetworkInterfaces
  let localIPv4 = fmap N.ipv4 localInterfaces
  process state handle localIPv4 getIPLocation 
#else
sniff :: Server State -> Server ()
sniff state = return ()
#endif

-- Ask the server for a new message, block until one arrives, repeat
-- Runs in separate thread on browser.
awaitLoop:: API -> [Message] -> Client ()
awaitLoop api chatlines = do
    withElem "chat" $ \chat -> do
        setProp chat "value" . unlines . map show . reverse $ take 100 chatlines
        getProp chat "scrollHeight" >>= setProp chat "scrollTop"
    msg <- onServer $ apiAwait api
    awaitLoop api $ msg : chatlines

-- | Client entry point.
clientMain :: API -> Client ()
clientMain api = do
  -- Tell the server we're here.
  onServer (apiHello api)

  -- Ask the server for a new message, block until one arrives, repeat
  fork $ awaitLoop api []


-- | Launch the application!
main :: IO ()
main = 
  -- Run the Haste.App application. Please note that a computation in the App
  -- monad should never contain any free variables.
  runApp (mkConfig "ws://192.168.1.194:24601" 24601) $ do
    -- Create our state-holding elements
    state <- liftServerIO $ newIORef []

    forkServerIO $ sniff state

    -- Create an API object holding all available functions
    api <- API <$> remote (hello state)
               <*> remote (await state)

    -- Launch the client
    runClient $ clientMain api
