module TcpPacket (
    TcpConnection(..),
    Packet(..),
    leadingPacket,
    nullPacket,
    fFin,
    fSyn,
    fAck,
    fReset,
    filterEthernet
) where

import Foreign
import Haste.App
import Network.Info


fFin   :: Word8
fSyn   :: Word8
fReset :: Word8
fAck   :: Word8

fFin   = 1
fSyn   = shift 1 1
fReset = shift 1 2
fAck   = shift 1 4

data TcpConnection = TcpConnection { 
                  sourceAddr :: IPv4,
                  sourcePort :: Word16,
                  destAddr :: IPv4,
                  destPort :: Word16
              }  deriving (Show)

-- helper function for comparing connections
comparisonTuple :: TcpConnection -> (Word32, Word16, Word32, Word16)
comparisonTuple (TcpConnection (IPv4 sa32) sp (IPv4 da32) dp) =
    if ((sa32 < da32) || sa32 == da32 && sp < dp) 
    then ( da32, dp, sa32, sp)
    else ( sa32, sp, da32, dp)

-- TcpConnection must be instance of Ord/Eq for set creation.
instance Eq TcpConnection where
  c0 == c1 = comparisonTuple c0 == comparisonTuple c1

instance Ord TcpConnection where
  c0 <= c1 = comparisonTuple c0 <= comparisonTuple c1

data Packet = Packet { 
                  connection :: TcpConnection,
                  flags      :: Word8
              }  deriving (Show)

-- IPv4 must be instance of Binary for server -> client communication.
instance Binary IPv4 where
  put (IPv4 w32) = put w32
  get = do
      w32 <- get
      return $ IPv4 w32

-- TcpConnection must be instance of Binary for server -> client communication.
instance Binary TcpConnection where
  put (TcpConnection sa sp da dp) = put sa >> put sp >> put da >> put dp 
  get = do
      da <- get
      dp <- get
      sa <- get
      sp <- get
      return $ TcpConnection sa sp da dp 

-- Packet must be instance of Binary for server -> client communication.
instance Binary Packet where
  put (Packet conn fl) = put conn >> put fl
  get = do
      conn <- get
      fl <- get
      return $ Packet conn fl

toPort :: Word8 -> Word8 -> Word16
toPort p1 p2 = fromIntegral p2 + shift (fromIntegral p1) 8

toIPv4 :: Word8 -> Word8 -> Word8 -> Word8 -> IPv4
toIPv4 a1 a2 a3 a4 =
    IPv4 (   shift (fromIntegral a4) 24
           + shift (fromIntegral a3) 16
           + shift (fromIntegral a2) 8
           +        fromIntegral a1
         )

nullTcpConnection :: TcpConnection
nullTcpConnection = TcpConnection (toIPv4 0 0 0 0) (toPort 0 0) (toIPv4 0 0 0 0) (toPort 0 0) 

nullPacket :: Packet
nullPacket = Packet nullTcpConnection 0

trailingFlags :: Word8 -> Bool
trailingFlags fl = ((fl .&. fFin) /= 0) || ((fl .&. fReset) /= 0)

leadingFlags :: Word8 -> Bool
leadingFlags fl = ((fl .&. fSyn) /= 0) && ((fl .&. fAck) /= 0)

leadingPacket :: Packet -> Bool
leadingPacket pkt = let fl = flags pkt in leadingFlags fl

filterEthernet:: [Word8] -> Maybe Packet
filterEthernet b =
    case b of
        (_:_:_:_:_:_:              -- dest mac
         _:_:_:_:_:_:              -- source mac
         0x81:0x00:_:_:            -- Recognize 802.1Q by tag : 0x8100
         typeByte1:typeByte0:      -- EtherType
         payload) -> filterPayload typeByte1 typeByte0 payload

        (_:_:_:_:_:_:              -- dest mac
         _:_:_:_:_:_:              -- source mac
         typeByte1:typeByte0:      -- EtherType
         payload) -> filterPayload typeByte1 typeByte0 payload

        _ -> Nothing

    where filterPayload typeByte1 typeByte2 payload = 
              if typeByte1*256+typeByte2 == 0x0800 -- IP protocol
                  then filterIP payload
              else Nothing

filterIP:: [Word8] -> Maybe Packet
filterIP b = 
    case b of
        (hl:_ :_ :_ :              -- hl: internet header length
         _ :_ :_ :_ :              -- fr: flags + fragmentOffset
         _ :6 :_ :_ :              -- pr: protocol ; 6 -> TCP
         s1:s2:s3:s4:              -- source ip
         d1:d2:d3:d4:              -- dest ip
         _) -> do 
                   tcp <- filterTCP $ drop (fromIntegral (4*(hl .&. 0xF))) b
                   let co = (connection tcp) 
                               { sourceAddr = toIPv4 s1 s2 s3 s4 ,
                                 destAddr   = toIPv4 d1 d2 d3 d4  }
                   Just $ tcp { connection = co }
        _ -> Nothing

filterTCP:: [Word8] -> Maybe Packet
filterTCP b = 
    case b of
        (s1:s2:d1:d2:              -- source port / dest port
         _ :_ :_ :_ :              -- sequence number 
         _ :_ :_ :_ :              -- acknowledgment number
         _ :fl:_ :_ :              -- fl: flags - 1 bit each
         _) -> if leadingFlags fl || trailingFlags fl 
                   then Just $ Packet (TcpConnection 
                                       (toIPv4 0 0 0 0) (toPort s1 s2 )
                                       (toIPv4 0 0 0 0) (toPort d1 d2 ))
                                          fl 
                   else Nothing
        _ -> Nothing
