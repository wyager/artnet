module Artnet.Data (Outgoing, fresh, add, finalize, Raw (..)) where

import Artnet (Data (..))
import Control.Monad (replicateM_, when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Serialize (Serialize)
import qualified Data.Serialize as Ser
import Prelude hiding (head)

newtype Outgoing = Outgoing (Map Int ByteString) deriving (Show)

-- For if you want to slap some bytestrings directly into the outgoing data.
-- Good for testing. Not a legal serialize instance.
newtype Raw = Raw ByteString

instance Serialize Raw where
  put (Raw bytes) = Ser.putByteString bytes
  get = return (Raw "")

fresh :: Outgoing
fresh = Outgoing Map.empty

add :: (MonadFail m, Serialize pixel, Traversable f) => Int -> f pixel -> Outgoing -> m Outgoing
add offset _ _ | offset < 0 = fail "Offset must be >=0"
add offset pixels (Outgoing chunks) = do
  case at of
    Nothing -> return ()
    Just _conflict -> fail $ "There is already a device starting at offset " ++ show offset
  case Map.maxViewWithKey below of
    Nothing -> return ()
    Just ((belowOff, belowChunk), _) ->
      when (belowOff + BS.length belowChunk > offset) $
        fail "The below device's data conflicts with this device."
  case Map.minViewWithKey above of
    Nothing -> return ()
    Just ((aboveOff, _), _) ->
      when (offset + BS.length encoded > aboveOff) $
        fail "This would clobber the above device's data"
  when (offset + BS.length encoded > 512) $
    fail "There is not enough room to put the data for this device"
  return $ Outgoing $ Map.insert offset encoded chunks
  where
    (below, at, above) = Map.splitLookup offset chunks
    encoded :: ByteString
    encoded = Ser.runPut $ mapM_ Ser.put pixels

finalize :: Outgoing -> Data
finalize (Outgoing chunks) = Data $ Ser.runPut $ go 0 (Map.toAscList chunks)
  where
    -- Must be even length
    go head [] = when (odd head) (Ser.putWord8 0)
    go head ((offset, chunk) : chunks') = do
      -- Add in filler, as needed
      replicateM_ (offset - head) (Ser.putWord8 0)
      Ser.putByteString chunk
      go (offset + BS.length chunk) chunks'
