{-# LANGUAGE OverloadedStrings, DerivingVia, LambdaCase, DeriveGeneric, DeriveAnyClass #-}
module Lib
    ( someFunc
    ) where

import Data.Serialize (Serialize, put, get, getByteString, putByteString,
    getWord8, getWord16le, putWord16le, putWord8, putWord16be, getWord16be)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics
import Control.Monad (guard)



newtype U16LE = U16LE {u16le :: Word16}
instance Serialize U16LE where
    put = putWord16le . u16le
    get = U16LE <$> getWord16le

newtype U16BE = U16BE {u16be :: Word16}
instance Serialize U16BE where
    put = putWord16be . u16be
    get = U16BE <$> getWord16be


newtype Sequence = Sequence Word8 deriving Serialize via Word8
newtype Physical = Physical Word8 deriving Serialize via Word8
newtype Flags = Flags Word8 deriving Serialize via Word8
newtype UBEAVersion = UBEAVersion Word8 deriving Serialize via Word8
newtype DiagPriority = DiagPriority Word8 deriving Serialize via Word8
newtype Universe = Universe Word16 deriving Serialize via U16LE
newtype TargetPortAddr = TargetPortAddr Word16 deriving Serialize via U16BE

newtype Data = Data ByteString
instance Serialize Data where
    put (Data bs) = do
        putWord16le $ fromIntegral (BS.length bs)
        putByteString bs
    get = do
        len <- getWord16le 
        Data <$> getByteString (fromIntegral len)


data IPv4 = IPv4 Word8 Word8 Word8 Word8 deriving (Generic, Serialize)
data MAC = MAC Word8 Word8 Word8 Word8 Word8 Word8 deriving (Generic, Serialize)
data UID = UID Word8 Word8 Word8 Word8 Word8 Word8 deriving (Generic, Serialize)
data PortTypes = PortTypes Word8 Word8 Word8 Word8 deriving (Generic, Serialize)
data GoodInput = GoodInput Word8 Word8 Word8 Word8 deriving (Generic, Serialize)
data GoodOutput = GoodOutput Word8 Word8 Word8 Word8 deriving (Generic, Serialize)
data SwIn = SwIn Word8 Word8 Word8 Word8 deriving (Generic, Serialize)
data SwOut = SwOut Word8 Word8 Word8 Word8 deriving (Generic, Serialize)
data Filler = Filler Word64 Word64 Word64 Word32 Word8 deriving (Generic, Serialize)

data Port6454 = Port6454

instance Serialize Port6454 where
    put Port6454 = putWord16le 0x1936
    get = do
        0x1936 <- getWord16le
        return Port6454

newtype VersInfo = VersInfo Word16 deriving Serialize via U16BE
newtype OEM = OEM Word16 deriving Serialize via U16BE
newtype ESTA = ESTA Word16 deriving Serialize via U16BE
newtype NumPorts = NumPorts Word16 deriving Serialize via U16BE

data Switch = Switch Word8 Word8 deriving (Generic, Serialize)
newtype Status  = Status Word8 deriving Serialize via Word8
newtype AcnPriority  = AcnPriority Word8 deriving Serialize via Word8
newtype SwMacro = SwMacro Word8 deriving Serialize via Word8
newtype SwRemote = SwRemote Word8 deriving Serialize via Word8
newtype Spare = Spare Word8 deriving Serialize via Word8
newtype Style = Style Word8 deriving Serialize via Word8
newtype BindIndex = BindIndex Word8 deriving Serialize via Word8

newtype ShortName = ShortName {getShortName :: ByteString }
instance Serialize ShortName where
    get = ShortName <$> getByteString 18
    put = putByteString . getShortName

newtype LongName = LongName {getLongName :: ByteString }
instance Serialize LongName where
    get = LongName <$> getByteString 64
    put = putByteString . getLongName

newtype NodeReport = NodeReport {getNodeReport :: ByteString }
instance Serialize NodeReport where
    get = NodeReport <$> getByteString 64
    put = putByteString . getNodeReport

data ArtPollReply_ = ArtPollReply_ IPv4 Port6454 VersInfo Switch OEM UBEAVersion Status ESTA ShortName LongName NodeReport NumPorts PortTypes GoodInput GoodOutput AcnPriority SwMacro SwRemote Spare Spare Spare Style MAC IPv4  BindIndex Status GoodOutput Status UID Filler deriving (Generic, Serialize)

data ArtPoll_ = ArtPoll_ Flags TargetPortAddr TargetPortAddr deriving (Generic, Serialize)

data ArtDMX_ = ArtDMX_ Sequence Physical Universe Data deriving (Generic, Serialize)

data ArtCommand = 
    -- A poll command | used to discover devices on the network
    ArtPoll  ArtPoll_ |

    -- A reply to the poll command | it contains device status information
    ArtPollReply  ArtPollReply_ |


    -- An ArtDmx data packet. Used to send actual data to a node in the network
    ArtDMX ArtDMX_

data Proto = V14
instance Serialize Proto where
    put V14 = putWord8 0 >> putWord8 14
    get = do
        0 <- getWord8
        14 <- getWord8
        return V14

data Opcode = OpArtDMX | OpArtPoll | OpArtPollReply


instance Serialize Opcode where
    put x = putWord16le $ case x of
        OpArtDMX -> 0x5000
        OpArtPoll -> 0x2000
        OpArtPollReply -> 0x2100
    get = getWord16le >>= \case
        0x5000 -> return OpArtDMX
        0x2000 -> return OpArtPoll
        0x2100 -> return OpArtPollReply
        x -> fail $ "Unknown opcode " ++ show x
header :: ByteString
header = "Art-Net\0"
instance Serialize ArtCommand where
    put cmd = do
        putByteString header
        case cmd of
            ArtDMX dmx -> put OpArtDMX >> put V14 >> put dmx
            ArtPoll poll -> put OpArtPoll >> put V14 >> put poll
            ArtPollReply apr -> put OpArtPoll >> put V14 >> put apr
    get = do
        guard . (header ==) =<< getByteString 8
        opcode <- get
        V14 <- get
        case opcode of
            OpArtPoll -> ArtPoll <$> get
            OpArtPollReply -> ArtPollReply <$> get 
            OpArtDMX -> ArtDMX <$> get 
        

someFunc :: IO ()
someFunc = putStrLn "someFunc"
