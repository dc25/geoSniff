module TcpPacket (
    TcpConnection(..),
    DNSAnswer(..),
    Packet(..),
    leadingPacket,
    nullPacket,
    filterEthernet
) where

import Foreign
import Haste.App
import Network.Info
import Data.Hash
import Data.Char (chr)


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

data DNSAnswer = DNSAnswer { 
                  name :: String,
                  address :: IPv4
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

instance Hashable TcpConnection where
  hash conn = hash $ comparisonTuple conn 

data Packet = Packet { 
                  connection :: TcpConnection,
                  flags      :: Word8
              }  |
              DNSPacket {
                  typeA :: [DNSAnswer]
              } deriving (Show)

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
      sa <- get
      sp <- get
      da <- get
      dp <- get
      return $ TcpConnection sa sp da dp 

-- Packet must be instance of Binary for server -> client communication.
instance Binary Packet where
  put (Packet conn fl) = put conn >> put fl
  get = do
      conn <- get
      fl <- get
      return $ Packet conn fl

toWord16 :: Word8 -> Word8 -> Word16
toWord16 w1 w2 = fromIntegral w2 + shift (fromIntegral w1) 8


toPort :: Word8 -> Word8 -> Word16
toPort = toWord16

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
              if toWord16 typeByte1 typeByte2 == 0x0800 -- IP protocol
                  then filterIP payload
              else Nothing

filterIP:: [Word8] -> Maybe Packet
filterIP b = 
    case b of
        (hl:_ :_ :_ :              -- hl: internet header length
         _ :_ :_ :_ :              -- fr: flags + fragmentOffset
         _ :pr:_ :_ :              -- pr: protocol ; 6 -> TCP ; 17 -> UDP
         s1:s2:s3:s4:              -- source ip
         d1:d2:d3:d4:              -- dest ip
         _) -> do 
            let payload = drop (fromIntegral (4*(hl .&. 0xF))) b
            let sa = toIPv4 s1 s2 s3 s4
            let da = toIPv4 d1 d2 d3 d4 
            if pr == 6 then do
                tcp <- filterTCP payload
                let co = (connection tcp) { sourceAddr = sa, destAddr = da }
                Just $ tcp { connection = co }
            else if pr == 17 then do
                filterUDP payload
            else
                Nothing
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

filterUDP:: [Word8] -> Maybe Packet
filterUDP b = 
    case b of
        (s1:s2:_ :_ :              -- source port / dest port
         _ :_ :_ :_ :              -- length / checksum
         payload) -> if toPort s1 s2 == 53 then -- DNS response
                         filterDNS payload
                     else 
                         Nothing
        _ -> Nothing

filterDNS:: [Word8] -> Maybe Packet
filterDNS b = 
    case b of
        (_ :_ :_ :_ :              -- id / flags
         q1:q2:a1:a2:              -- question count / answer count
         _ :_ :_ :_ :              -- authority count / additional count
         payload) -> do
             let questionCount = toWord16 q1 q2
             let answerCount   = toWord16 a1 a2
             let answerPayload = skipDNSQuestions questionCount payload
             let answers = readDNSAnswers answerCount answerPayload b
             Just $ DNSPacket answers
        _ -> Nothing

skipDNSQuestions:: Word16 -> [Word8] -> [Word8]
skipDNSQuestions 0 b = b 
skipDNSQuestions questionCount b = skipDNSQuestions (questionCount - 1) $ skipOneDNSQuestion b

skipOneDNSQuestion:: [Word8] -> [Word8]
skipOneDNSQuestion b = drop 4 $ skipDNSString b -- 4 = type(2) + class(2)

skipDNSString :: [Word8] -> [Word8]
skipDNSString (0:finalAnswer) = finalAnswer
skipDNSString (n:label) = 
    if (n .&. 0xC0 == 0xC0) -- DNS string compression
    then drop 1 label  -- compressed
    else skipDNSString $ drop (fromIntegral n) label  -- skip length n label and continue
skipDNSString [] = []

readDNSAnswers:: Word16 -> [Word8] -> [Word8] -> [DNSAnswer]
readDNSAnswers 0 _ _ = []
readDNSAnswers answerCount b dnsData = 
    let queryString = readDNSString b dnsData
        afterString = skipDNSString b
        (t1:t2: afterType) = afterString
        (_:_: _:_:_:_: dataLength1:dataLength2: answerData) = afterType
        dataType = toWord16 t1 t2
    in  if dataType == 1 then -- type A record
            let (a1:a2:a3:a4: nextAnswerData ) = answerData
                thisAnswer = DNSAnswer queryString (toIPv4 a1 a2 a3 a4)
            in thisAnswer : readDNSAnswers (answerCount - 1) nextAnswerData dnsData
        else -- just skip over anything other than type A
            let dataLength = toWord16 dataLength1 dataLength2
            in readDNSAnswers (answerCount - 1) (drop (fromIntegral dataLength) answerData) dnsData

readDNSString :: [Word8] -> [Word8] -> String
readDNSString [] _ = []
readDNSString (0:_) _ = []
readDNSString (n1:labelData) dnsData = 
    if (n1 .&. 0xC0 == 0xC0) -- DNS string compression
    then 
        let n2 = head labelData
            stringOffset = toWord16 (n1 .&. 0x3F) n2
        in readDNSString (drop (fromIntegral stringOffset) dnsData) dnsData
    else 
        let labelLength = fromIntegral n1
            label = map (chr . fromEnum) (take labelLength labelData)
            
        in  label ++ "." ++ readDNSString (drop labelLength labelData) dnsData -- read length n label and continue

