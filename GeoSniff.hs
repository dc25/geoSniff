{-# LANGUAGE CPP #-}
module GeoSniff (
    launchApp
) where


-- | A utility for doing live display of tcp connections in the browser.
-- | Started out as a copy of the Haste.App chatbox demo program.

import Haste.App
import Haste.App.Concurrent
import qualified Haste.Prim as HP
import qualified Control.Concurrent as C
import Control.Monad
import Control.Applicative
import Data.IORef
import Data.Hash
import qualified Data.Foldable as DF
import qualified Data.Set as S
import qualified Data.Map as M

import Network.Pcap
import qualified Network.Info as N
import Packet
import qualified Foreign as F 
#ifndef __HASTE__
import LocateIP
#endif

#ifdef __HASTE__ 
-- foreign functionality only compiles in haste client due
-- to conflict with "import Foreign"

foreign import ccall placeMarker_ffi :: HP.JSString -> HP.JSString -> Int -> HP.JSString -> Int -> Double -> Double -> IO ()
foreign import ccall removeMarker_ffi :: HP.JSString -> IO ()

#else
-- dummies ; unused by server

placeMarker_ffi :: HP.JSString -> HP.JSString -> Int -> HP.JSString -> Int -> Double -> Double -> IO ()
placeMarker_ffi _ _ _ _ _ _ _ = do return ()

removeMarker_ffi :: HP.JSString -> IO ()
removeMarker_ffi _  = do return ()

#endif

-- A server to client message:
data Message = Message Packet String Double Double String deriving Show

-- Message must be instance of Binary for server -> client communication.
instance Binary Message where
    put (Message pkt hsh long lat rmt) = put pkt >> put hsh >> put long >> put lat >> put rmt
    get = do
        pk <- get
        hsh <- get
        lo <- get
        la <- get
        rmt <- get
        return $ Message pk hsh lo la rmt

nullMessage :: Message
nullMessage = Message nullPacket "" 0.0 0.0 ""

-- | The type representing our clients - a list matching active clients with
--   the MVars used to notify them of a new message, and a backlog of messages.
type CurrentClients = IORef [(SessionID, C.MVar Message)]

-- | Data type to hold all our API calls
data API = API {
    apiHello :: Remote (Server ()),
    apiAwait :: Remote (Server Message)
}

-- | Tell the server we're here and remove any stale sessions.
hello :: Server CurrentClients -> Server ()
hello clients = do
    sid <- getSessionID
    active <- getActiveSessions
    cl <- clients
    liftIO $ do
        v <- C.newEmptyMVar
        atomicModifyIORef cl $ \cs ->
          ((sid, v) : filter (\(sess, _) -> sess `S.member` active) cs, ())
        return ()

-- | Send a message by notifying everyone who is waiting for a message.
send :: Server CurrentClients -> Message -> Server ()
send clients msg = do
    cl <- clients
    liftIO $ do
        cs <- readIORef cl
        -- Fork a new thread for each MVar so slow clients don't hold up fast ones.
        forM_ cs $ \(_, v) -> C.forkIO $ C.putMVar v msg

-- | Block until a new message arrives, then return it.
await :: Server CurrentClients -> Server Message
await clients = do
    sid <- getSessionID
    cl <- clients
    liftIO $ readIORef cl >>= maybe (return nullMessage) C.takeMVar . lookup sid

#ifndef __HASTE__

type IPLookupFunction = (N.IPv4 -> IO IPLookupResults)
type DNSMap = M.Map N.IPv4 String

remoteName :: DNSMap -> N.IPv4 -> String
remoteName dnsMap ip = 
    case M.lookup ip dnsMap of 
        Just n -> n
        Nothing -> show ip

sniffLoop :: Server CurrentClients -> PcapHandle -> [N.IPv4] -> S.Set TcpConnection -> IPLookupFunction  -> DNSMap -> Server ()
sniffLoop clients handle localIPv4 liveConnections getLocation dnsMap = do

    -- partial application ; 3 arguments unchanged
    let keepGoing = sniffLoop clients handle localIPv4 

    -- Get raw data from the network
    (hdr,pkt) <- liftIO $ Network.Pcap.next handle
    bytes <- liftIO $ F.peekArray (fromIntegral (hdrCaptureLength hdr)) pkt

    -- Convert the raw data into records that describe events of interest.
    case processEthernet bytes of 
        Just (DNSPacket answers) ->  
            -- If we got a DNS record containing a list of answers, "fold" it
            -- into the current map and continue.
            let newDnsMap = DF.foldl' (\m a-> M.insert (address a) (name a) m) dnsMap answers
            in keepGoing liveConnections getLocation newDnsMap
        Just packet@(TcpPacket conn@(TcpConnection sa _ da _) _) -> 
            if leadingPacket packet then 
                if (S.notMember conn liveConnections) then do
                    let newLiveConnections = S.insert conn liveConnections
                    let remoteIp = if sa `elem` localIPv4 then da else sa 
                    IPLookupResults maybeLoc newGetLocation <- liftIO $ getLocation remoteIp
                    case maybeLoc of
                        Just (Location la lo) -> do
                            send clients $ Message packet (show $ asWord64 $ hash conn) la lo (remoteName dnsMap remoteIp)
                            keepGoing newLiveConnections newGetLocation dnsMap
                        Nothing -> keepGoing newLiveConnections newGetLocation dnsMap
                else 
                    keepGoing liveConnections getLocation  dnsMap
            else -- not leading so must be trailing
                if S.member conn liveConnections then do
                    let newLiveConnections = S.delete conn liveConnections
                    send clients $ Message packet (show $ asWord64 $ hash conn) 0.0 0.0 ""
                    keepGoing newLiveConnections getLocation  dnsMap
                else
                    keepGoing liveConnections getLocation  dnsMap
        Nothing -> keepGoing liveConnections getLocation  dnsMap

sniff :: Server CurrentClients -> Server ()
sniff clients = do
    handle <- liftIO $ openLive "wlan0" 400 False 1000000
    localInterfaces <- liftIO N.getNetworkInterfaces
    let localIPv4 = fmap N.ipv4 localInterfaces
    sniffLoop clients handle localIPv4 S.empty getIPLocation M.empty
#else
sniff :: Server CurrentClients -> Server ()
sniff clients = return ()
#endif

-- Ask the server for a new message, block until one arrives, repeat
-- Runs in separate thread on browser.
awaitLoop:: API -> Client ()
awaitLoop api = do
    Message pkt@(TcpPacket (TcpConnection _ sp da dp) _) hsh la lo rname <- onServer $ apiAwait api
    if leadingPacket pkt then
        liftIO $ placeMarker_ffi (HP.toJSStr hsh) (HP.toJSStr $ show da) (fromIntegral dp) (HP.toJSStr $ rname) (fromIntegral sp) la lo
    else 
        liftIO $ removeMarker_ffi (HP.toJSStr hsh) 
    awaitLoop api 

-- | Client entry point.
clientMain :: API -> Client ()
clientMain api = do
    -- Tell the server we're here.
    onServer (apiHello api)

    -- Ask the server for a new message, block until one arrives, repeat
    fork $ awaitLoop api 


-- | Launch the application!
launchApp :: String -> Int -> IO ()
launchApp serviceAddress servicePort = 
    -- Run the Haste.App application. Please note that a computation in the App
    -- monad should never contain any free variables.
    runApp (mkConfig serviceAddress servicePort ) $ do
        -- Create our clients-holding elements
        clients <- liftServerIO $ newIORef []

        forkServerIO $ sniff clients

        -- Create an API object holding all available functions
        api <- API <$> remote (hello clients)
                   <*> remote (await clients)

        -- Launch the client
        runClient $ clientMain api

