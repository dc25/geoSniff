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


-- | A chat message consists of a sender name and a message.
type Message = (String, String)

-- | The type representing our state - a list matching active clients with
--   the MVars used to notify them of a new message, and a backlog of messages.
type State = (IORef [(SessionID, C.MVar Message)], IORef [Message])

-- | Data type to hold all our API calls
data API = API {
    apiHello :: Remote (Server [Message]),
    apiSend  :: Remote (String -> String -> Server ()),
    apiAwait :: Remote (Server Message)
  }

-- | Tell the server we're here and remove any stale sessions.
hello :: Server State -> Server [Message]
hello state = do
  sid <- getSessionID
  active <- getActiveSessions
  (clients, messages) <- state
  liftIO $ do
    v <- C.newEmptyMVar
    atomicModifyIORef clients $ \cs ->
      ((sid, v) : filter (\(sess, _) -> sess `S.member` active) cs, ())
    readIORef messages

-- | Send a message; keep a backlog of 100 messages.
send :: Server State -> String -> String -> Server ()
send state sender msg = do
  (clients, messages) <- state
  liftIO $ do
    cs <- readIORef clients
    atomicModifyIORef messages $ \msgs -> ((sender, msg):take 99 msgs, ())
    -- Fork a new thread for each MVar so slow clients don't hold up fast ones.
    forM_ cs $ \(_, v) -> C.forkIO $ C.putMVar v (sender, msg)

-- | Block until a new message arrives, then return it.
await :: Server State -> Server Message
await state = do
  sid <- getSessionID
  (clients, _) <- state
  liftIO $ readIORef clients >>= maybe (return ("","")) C.takeMVar . lookup sid

#ifndef __HASTE__
process :: Server State -> PcapHandle -> [N.IPv4] -> (N.IPv4 -> IO IPLookupResults) -> Server ()
process state handle localIPv4 getIPLocation' = do
    (hdr,pkt) <- liftIO $ Network.Pcap.next handle
    bytes <- liftIO $ peekArray (fromIntegral (hdrCaptureLength hdr)) pkt
    case filterEthernet bytes of 
        Just packet@(Packet sa _ da _ _) -> do
            let remoteIp = if sa `elem` localIPv4 then da else sa 
            lookupResults  <- liftIO $ getIPLocation' remoteIp
            send state "sniff" $ show packet
            send state "sniff location" $ show $ location lookupResults
            process state handle localIPv4 (lookupFunction lookupResults) -- loop forever

        _ -> process state handle localIPv4 getIPLocation' -- loop forever
#endif

sniff :: Server State -> Server ()
sniff state = do
  handle <- liftIO $ openLive "wlan0" 500 False 0
  localInterfaces <- liftIO N.getNetworkInterfaces
  let localIPv4 = fmap N.ipv4 localInterfaces
#ifndef __HASTE__
  process state handle localIPv4 getIPLocation 
#else
  return ()
#endif

-- | Scroll to the bottom of a textarea.
scrollToBottom :: Elem -> Client ()
scrollToBottom el = getProp el "scrollHeight" >>= setProp el "scrollTop"

-- Ask the server for a new message, block until one arrives, repeat
-- Runs in separate thread on browser.
awaitLoop:: API -> Elem -> [String] -> Client ()
awaitLoop api chat chatlines = do
    setProp chat "value" . unlines . reverse $ take 100 chatlines
    scrollToBottom chat
    (from, msg) <- onServer $ apiAwait api
    awaitLoop api chat $ (from ++ ": " ++ msg) : chatlines

-- | Client entry point.
clientMain :: API -> Client ()
clientMain api = withElems ["name","message","chat"] $ \[name, msg, chat] -> do
  -- Tell the server we're here, and fill out our backlog.
  backlog <- map (\(n, m) -> n ++ ": " ++ m) <$> onServer (apiHello api)

  -- Ask the server for a new message, block until one arrives, repeat
  fork $ awaitLoop api chat backlog

  -- Send a message if the user hits return (charcode 13)
  msg `onEvent` OnKeyDown $ \k -> 
    case k of
      13 -> do
        m <- getProp msg "value"
        n <- getProp name "value"
        setProp msg "value" ""
        onServer $ apiSend api <.> (n :: String) <.> (m :: String)
      _ -> 
        return ()
  return ()


-- | Launch the application!
main :: IO ()
main = 
  -- Run the Haste.App application. Please note that a computation in the App
  -- monad should never contain any free variables.
  runApp (mkConfig "ws://192.168.1.194:24601" 24601) $ do
    -- Create our state-holding elements
    state <- liftServerIO $ do
      clients <- newIORef []
      messages <- newIORef []
      return (clients,messages)

    forkServerIO $ sniff state

    -- Create an API object holding all available functions
    api <- API <$> remote (hello state)
               <*> remote (send state)
               <*> remote (await state)

    -- Launch the client
    runClient $ clientMain api
