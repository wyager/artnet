{-# LANGUAGE OverloadedStrings, DerivingVia, LambdaCase, DeriveGeneric, DeriveAnyClass #-}
module Lib
    (ArtCommand(..), ArtPoll_(..), ArtPollReply_(..), ArtDMX_(..)
     
    ) where

import Data.Serialize (Serialize, put, get, getByteString, putByteString,
    getWord8, getWord16le, putWord16le, putWord8, putWord16be, getWord16be)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Word (Word8, Word16, Word32, Word64)
import GHC.Generics
import Control.Monad (guard)
import Data.Bits (Bits, testBit)



newtype U16LE = U16LE {u16le :: Word16} deriving Show
instance Serialize U16LE where
    put = putWord16le . u16le
    get = U16LE <$> getWord16le

newtype U16BE = U16BE {u16be :: Word16} deriving Show
instance Serialize U16BE where
    put = putWord16be . u16be
    get = U16BE <$> getWord16be


newtype Sequence = Sequence Word8 deriving Serialize via Word8 deriving Show
newtype Physical = Physical Word8 deriving Serialize via Word8 deriving Show
newtype Flags = Flags Word8 deriving (Serialize, Eq, Bits, Num) via Word8 deriving Show
flagsTargeted :: Flags -> Bool
flagsTargeted = (`testBit` 5)
newtype UBEAVersion = UBEAVersion Word8 deriving Serialize via Word8 deriving Show
newtype DiagPriority = DiagPriority Word8 deriving Serialize via Word8 deriving Show
newtype Universe = Universe Word16 deriving Serialize via U16LE deriving Show
newtype TargetPortAddr = TargetPortAddr Word16 deriving Serialize via U16BE deriving Show

newtype Data = Data ByteString deriving Show
instance Serialize Data where
    put (Data bs) = do
        putWord16be $ fromIntegral (BS.length bs)
        putByteString bs
    get = do
        len <- getWord16be
        Data <$> getByteString (fromIntegral len)


data IPv4 = IPv4 Word8 Word8 Word8 Word8 deriving (Generic, Serialize, Show)
data MAC = MAC Word8 Word8 Word8 Word8 Word8 Word8 deriving (Generic, Serialize, Show)
data UID = UID Word8 Word8 Word8 Word8 Word8 Word8 deriving (Generic, Serialize, Show)
data PortTypes = PortTypes Word8 Word8 Word8 Word8 deriving (Generic, Serialize, Show)
data GoodInput = GoodInput Word8 Word8 Word8 Word8 deriving (Generic, Serialize, Show)
data GoodOutput = GoodOutput Word8 Word8 Word8 Word8 deriving (Generic, Serialize, Show)
data SwIn = SwIn Word8 Word8 Word8 Word8 deriving (Generic, Serialize, Show)
data SwOut = SwOut Word8 Word8 Word8 Word8 deriving (Generic, Serialize, Show)
data Filler = Filler Word64 Word64 Word64 Word32 Word8 deriving (Generic, Serialize, Show)

data Port6454 = Port6454 deriving Show

instance Serialize Port6454 where
    put Port6454 = putWord16le 0x1936
    get = do
        0x1936 <- getWord16le
        return Port6454

newtype VersInfo = VersInfo Word16 deriving Serialize via U16BE deriving Show
newtype OEM = OEM Word16 deriving Serialize via U16BE deriving Show
newtype ESTA = ESTA Word16 deriving Serialize via U16BE deriving Show
newtype NumPorts = NumPorts Word16 deriving Serialize via U16BE deriving Show

data Switch = Switch Word8 Word8 deriving (Generic, Serialize, Show)
newtype Status  = Status Word8 deriving Serialize via Word8 deriving Show
newtype AcnPriority  = AcnPriority Word8 deriving Serialize via Word8 deriving Show
newtype SwMacro = SwMacro Word8 deriving Serialize via Word8 deriving Show
newtype SwRemote = SwRemote Word8 deriving Serialize via Word8 deriving Show
newtype Spare = Spare Word8 deriving Serialize via Word8 deriving Show
newtype Style = Style Word8 deriving Serialize via Word8 deriving Show
newtype BindIndex = BindIndex Word8 deriving Serialize via Word8 deriving Show

newtype ShortName = ShortName {getShortName :: ByteString } deriving Show
instance Serialize ShortName where
    get = ShortName <$> getByteString 18
    put = putByteString . getShortName

newtype LongName = LongName {getLongName :: ByteString } deriving Show
instance Serialize LongName where
    get = LongName <$> getByteString 64
    put = putByteString . getLongName

newtype NodeReport = NodeReport {getNodeReport :: ByteString } deriving Show
instance Serialize NodeReport where
    get = NodeReport <$> getByteString 64
    put = putByteString . getNodeReport

data ArtPollReply_ = ArtPollReply_ IPv4 Port6454 VersInfo Switch OEM UBEAVersion Status ESTA ShortName LongName NodeReport NumPorts PortTypes GoodInput GoodOutput AcnPriority SwMacro SwRemote Spare Spare Spare Style MAC IPv4  BindIndex Status GoodOutput Status UID Filler deriving (Generic, Serialize, Show)

data ArtPoll_ = ArtPoll_ Flags (Maybe (TargetPortAddr, TargetPortAddr)) deriving (Show)
instance Serialize ArtPoll_ where
    get = do
        flags <- get
        target <- if flagsTargeted flags
            then Just <$> ((,) <$> get <*> get)
            else return Nothing
        return $ ArtPoll_ flags target
    -- TODO: Break out flags so we can't represent a thing with
    -- targeted = false and targets = Just ...
    put (ArtPoll_ f t) = put f >> mapM_ (\(a,b) -> put a >> put b) t
        
        

data ArtDMX_ = ArtDMX_ Sequence Physical Universe Data deriving (Generic, Serialize, Show)

data ArtCommand = 
    -- A poll command | used to discover devices on the network
    ArtPoll  ArtPoll_ |

    -- A reply to the poll command | it contains device status information
    ArtPollReply  ArtPollReply_ |


    -- An ArtDmx data packet. Used to send actual data to a node in the network
    ArtDMX ArtDMX_
    deriving Show

data Proto = V14 deriving Show
instance Serialize Proto where
    put V14 = putWord8 0 >> putWord8 14
    get = do
        0 <- getWord8
        14 <- getWord8
        return V14

data Opcode = OpArtDMX | OpArtPoll | OpArtPollReply deriving Show


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
        

