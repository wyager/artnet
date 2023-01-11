module Artnet
  ( ArtCommand (..),
    ArtPoll_ (..),
    defaultArtPoll,
    defaultArtPollReply,
    ArtPollReply_ (..),
    ArtDMX_ (..),
    Data (..),
    Universe (..),
  )
where

import Control.Monad (guard)
import Data.Bits (bit, testBit, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Maybe (isJust)
import Data.Serialize
  ( Serialize,
    get,
    getByteString,
    getWord16be,
    getWord16le,
    getWord8,
    put,
    putByteString,
    putWord16be,
    putWord16le,
    putWord8,
  )
import Data.Word (Word16, Word64, Word8)
import GHC.Generics

newtype U16LE = U16LE {u16le :: Word16}
  deriving (Show)
  deriving newtype (Num)

instance Serialize U16LE where
  put = putWord16le . u16le
  get = U16LE <$> getWord16le

newtype Sequence = Sequence Word8
  deriving newtype (Serialize, Num)
  deriving (Show)

newtype Physical = Physical Word8
  deriving newtype (Serialize, Num)
  deriving (Show)

newtype UBEAVersion = UBEAVersion Word8
  deriving newtype (Serialize)
  deriving (Show)

newtype DiagPriority = DiagPriority Word8
  deriving newtype (Serialize, Num)
  deriving (Show)

newtype Universe = Universe Word16
  deriving (Serialize, Num) via U16LE
  deriving (Show)

newtype TargetPortAddr = TargetPortAddr Word16
  deriving newtype (Serialize, Num)
  deriving (Show)

newtype Data = Data ByteString deriving (Show)

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

-- Spec says 15 bytes, but from wireshark it looks like 16? Just leaving it as 16
data Filler = Filler Word64 Word64 deriving (Generic, Serialize, Show)

data Port6454 = Port6454 deriving (Show)

instance Serialize Port6454 where
  put Port6454 = putWord16le 0x1936
  get = do
    0x1936 <- getWord16le
    return Port6454

newtype VersInfo = VersInfo Word16
  deriving newtype (Serialize)
  deriving (Show)

newtype OEM = OEM Word16
  deriving newtype (Serialize)
  deriving (Show)

newtype ESTA = ESTA Word16
  deriving newtype (Serialize)
  deriving (Show)

newtype NumPorts = NumPorts Word16
  deriving newtype (Serialize)
  deriving (Show)

data Switch = Switch Word8 Word8 deriving (Generic, Serialize, Show)

newtype Status = Status Word8
  deriving newtype (Serialize)
  deriving (Show)

newtype AcnPriority = AcnPriority Word8
  deriving newtype (Serialize)
  deriving (Show)

newtype SwMacro = SwMacro Word8
  deriving newtype (Serialize)
  deriving (Show)

newtype SwRemote = SwRemote Word8
  deriving newtype (Serialize)
  deriving (Show)

newtype Spare = Spare Word8
  deriving newtype (Serialize)
  deriving (Show)

newtype Style = Style Word8
  deriving newtype (Serialize)
  deriving (Show)

newtype BindIndex = BindIndex Word8
  deriving newtype (Serialize)
  deriving (Show)

newtype ShortName = ShortName {getShortName :: ByteString} deriving (Show)

instance Serialize ShortName where
  get = ShortName <$> getByteString 18
  put = putByteString . getShortName

newtype LongName = LongName {getLongName :: ByteString} deriving (Show)

instance Serialize LongName where
  get = LongName <$> getByteString 64
  put = putByteString . getLongName

newtype NodeReport = NodeReport {getNodeReport :: ByteString} deriving (Show)

instance Serialize NodeReport where
  get = NodeReport <$> getByteString 64
  put = putByteString . getNodeReport

data ArtPollReply_ = ArtPollReply_
  { nodeIP :: IPv4,
    nodePort :: Port6454,
    nodeVersion :: VersInfo,
    nodeSwitch :: Switch,
    nodeOEM :: OEM,
    nodeUBEA :: UBEAVersion,
    nodeStatus1 :: Status,
    nodeESTA :: ESTA,
    nodeShortName :: ShortName,
    nodeLongName :: LongName,
    nodeReport :: NodeReport,
    nodeNumPorts :: NumPorts,
    nodePortTypes :: PortTypes,
    nodeInputStatus :: GoodInput,
    nodeOutputStatus :: GoodOutput,
    nodeSwIn :: SwIn,
    nodeSwOut :: SwOut,
    nodeSACNPriority :: AcnPriority,
    nodeSwMacro :: SwMacro,
    nodeSwRemote :: SwRemote,
    nodeSpare1 :: Spare,
    nodeSpare2 :: Spare,
    nodeSpare3 :: Spare,
    nodeStyle :: Style,
    nodeMAC :: MAC,
    nodeBindIP :: IPv4,
    nodeBindIndex :: BindIndex,
    nodeStatus2 :: Status,
    nodeGoodOutputB :: GoodOutput,
    nodeStatus3 :: Status,
    nodeRDMNetUID :: UID,
    nodeFiller :: Filler
  }
  deriving (Generic, Serialize, Show)

defaultArtPollReply :: ArtPollReply_
defaultArtPollReply =
  ArtPollReply_
    (IPv4 1 2 3 4)
    Port6454
    (VersInfo 0)
    (Switch 0 0)
    (OEM 0)
    (UBEAVersion 0)
    (Status 0)
    (ESTA 0)
    (ShortName "0123456789abcdefgh")
    (LongName $ BS.concat ["xy" | _ <- [0 .. 31 :: Int]])
    (NodeReport $ BS.concat ["ab" | _ <- [0 .. 31 :: Int]])
    (NumPorts 0)
    (PortTypes 0 0 0 0)
    (GoodInput 0 0 0 0)
    (GoodOutput 0 0 0 0)
    (SwIn 0 0 0 0)
    (SwOut 0 0 0 0)
    (AcnPriority 0)
    (SwMacro 0)
    (SwRemote 0)
    (Spare 1)
    (Spare 2)
    (Spare 3)
    (Style 0)
    (MAC 1 2 3 4 5 6)
    (IPv4 7 8 9 10)
    (BindIndex 0)
    (Status 0)
    (GoodOutput 0 0 0 0)
    (Status 0)
    (UID 11 12 13 14 15 16)
    (Filler 1 2)

data ArtPoll_ = ArtPoll_ {disableVLC :: Bool, diagUnicast :: Bool, sendMeDiag :: Bool, sendMeContinuousReplies :: Bool, diagPriority :: DiagPriority, target :: Maybe (TargetPortAddr, TargetPortAddr)} deriving (Show)

defaultArtPoll :: ArtPoll_
defaultArtPoll =
  ArtPoll_
    { disableVLC = False,
      diagUnicast = False,
      sendMeDiag = False,
      sendMeContinuousReplies = False,
      diagPriority = 0,
      target = Nothing
    }

instance Serialize ArtPoll_ where
  get = do
    flags <- getWord8
    let test = testBit flags
        targeted = test 5
        disableVLC = test 4
        diagUnicast = test 3
        sendMeDiag = test 2
        sendMeContinuousReplies = test 1
    diagPriority <- get
    target <-
      if targeted
        then Just <$> ((,) <$> get <*> get)
        else return Nothing
    return $ ArtPoll_ {..}

  -- TODO: Break out flags so we can't represent a thing with
  -- targeted = false and targets = Just ...
  put (ArtPoll_ disableVLC diagUnicast sendMeDiag sendMeContinuousReplies diagPriority target) = do
    let set n cond = if cond then bit n else 0
        flags =
          foldl
            (.|.)
            (0 :: Word8)
            [ set 5 (isJust target),
              set 4 disableVLC,
              set 3 diagUnicast,
              set 2 sendMeDiag,
              set 1 sendMeContinuousReplies
            ]
    put flags >> put diagPriority >> mapM_ (\(a, b) -> put a >> put b) target

data ArtDMX_ = ArtDMX_ Sequence Physical Universe Data deriving (Generic, Serialize, Show)

data ArtCommand
  = -- A poll command | used to discover devices on the network
    ArtPoll ArtPoll_
  | -- A reply to the poll command | it contains device status information
    ArtPollReply ArtPollReply_
  | -- An ArtDmx data packet. Used to send actual data to a node in the network
    ArtDMX ArtDMX_
  deriving (Show)

data ProtoVersion = V14 deriving (Show)

instance Serialize ProtoVersion where
  put V14 = putWord8 0 >> putWord8 14
  get = do
    0 <- getWord8
    14 <- getWord8
    return V14

data Opcode = OpArtDMX | OpArtPoll | OpArtPollReply deriving (Show)

instance Serialize Opcode where
  put x = putWord16le $ case x of
    OpArtDMX -> 0x5000
    OpArtPoll -> 0x2000
    OpArtPollReply -> 0x2100
  get =
    getWord16le >>= \case
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
      ArtPollReply apr -> put OpArtPollReply >> put apr
  get = do
    let v14 = do V14 <- get; return ()
    guard . (header ==) =<< getByteString 8
    opcode <- get
    case opcode of
      OpArtPoll -> v14 >> ArtPoll <$> get
      OpArtPollReply -> ArtPollReply <$> get
      OpArtDMX -> v14 >> ArtDMX <$> get
