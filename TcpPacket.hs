module TcpPacket (
    Packet(..),
    terminalPacket,
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

data Packet = Packet { 
                  sourceAddr :: IPv4,
                  sourcePort :: Word16,
                  destAddr :: IPv4,
                  destPort :: Word16,
                  flags    :: Word8
              }  deriving (Show)

-- IPv4 must be instance of Binary for server -> client communication.
instance Binary IPv4 where
  put (IPv4 w32) = put w32
  get = do
      w32 <- get
      return $ IPv4 w32

-- Packet must be instance of Binary for server -> client communication.
instance Binary Packet where
  put (Packet sa sp da dp fl) = put sa >> put sp >> put da >> put dp >> put fl
  get = do
      da <- get
      dp <- get
      sa <- get
      sp <- get
      fl <- get
      return $ Packet sa sp da dp fl

toPort :: Word8 -> Word8 -> Word16
toPort p1 p2 = fromIntegral p2 + shift (fromIntegral p1) 8

toIPv4 :: Word8 -> Word8 -> Word8 -> Word8 -> IPv4
toIPv4 a1 a2 a3 a4 =
    IPv4 (   shift (fromIntegral a4) 24
           + shift (fromIntegral a3) 16
           + shift (fromIntegral a2) 8
           +        fromIntegral a1
         )

nullPacket :: Packet
nullPacket = Packet (toIPv4 0 0 0 0) (toPort 0 0) (toIPv4 0 0 0 0) (toPort 0 0) 0

terminalFlags :: Word8 -> Bool
terminalFlags fl = ((fl .&. fFin) /= 0) || ((fl .&. fReset) /= 0)

sentinalFlags :: Word8 -> Bool
sentinalFlags fl = ((fl .&. fSyn) /= 0) && ((fl .&. fAck) /= 0)

terminalPacket :: Packet -> Bool
terminalPacket pkt = let fl = flags pkt in terminalFlags fl

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
                   Just $ tcp { sourceAddr = toIPv4 s1 s2 s3 s4 ,
                                destAddr = toIPv4 d1 d2 d3 d4  }
         
        _ -> Nothing

filterTCP:: [Word8] -> Maybe Packet
filterTCP b = 
    case b of
        (s1:s2:d1:d2:              -- source port / dest port
         _ :_ :_ :_ :              -- sequence number 
         _ :_ :_ :_ :              -- acknowledgment number
         _ :fl:_ :_ :              -- fl: flags - 1 bit each
         _) -> if sentinalFlags fl || terminalFlags fl 
                   then Just $ Packet (toIPv4 0 0 0 0) (toPort s1 s2 )
                                      (toIPv4 0 0 0 0) (toPort d1 d2 )
                                          fl 
                   else Nothing
        _ -> Nothing
