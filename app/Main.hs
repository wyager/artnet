module Main (main) where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan as Chan
import Control.Concurrent (threadDelay)
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)
import qualified Lib
import qualified Lib.Data as Data
import qualified Lib.Pixel as Px
import Options.Generic (ParseRecord, getRecord)
import qualified Server

packets :: [Lib.ArtCommand]
packets = [poll, reply]
  where
    poll = Lib.ArtPoll Lib.defaultArtPoll
    reply = Lib.ArtPollReply Lib.defaultArtPollReply
   
data Opts = Opts {broadcastAddr :: String, localAddr :: String} deriving (Generic, Show, ParseRecord)

colorizer :: (Lib.ArtCommand -> IO ()) -> IO void
colorizer send = go $ cycle $ (Px.rounded . px) <$> temps
    where
    go :: [Px.CCTRGBWPx Word8 Word16] -> IO void
    go [] = error "wtf"
    go (_:colors) = do
        let decimate xs = head xs : decimate (drop 10 xs) 
            tube1 = take 16 $ decimate colors
            tube2 = reverse $ take 16 $ drop 16 $ decimate colors
            packet = case Data.finalize <$> (Data.add 0 tube1 Data.fresh >>= Data.add 256 tube2) of
                Nothing -> error "Couldn't fit data in DMX packet"
                Just pkt -> pkt
            cmd = Lib.ArtDMX $ Lib.ArtDMX_ 0 0 0 packet
        send cmd
        threadDelay 10_000
        go colors
    -- Create a CCT/RGBW pixel with Double [0,1] values
    px cct = Px.CCTRGBWPx @Double @Double 0.7 (Px.Temp cct) 0 0 (Px.RGBW 0 0 0 0)
    -- 10x oversampled
    temps = let ts = [0, (1 / (15 * 10)) .. 1] in ts ++ reverse ts

    

main :: IO ()
main = do
  Opts {..} <- getRecord "Server"
  chan <- Chan.newChan
  (listener, teller) <- Server.mkServers (either putStrLn print) (Chan.readChan chan) broadcastAddr localAddr
  mapM_ (Chan.writeChan chan) packets
  Async.withAsync (colorizer (Chan.writeChan chan)) $ \colored -> 
    Async.withAsync listener $ \listened -> do
      Async.withAsync teller $ \told -> do
        (_, err) <-
          Async.waitAny
            [ "Listener died" <$ listened,
              "Teller died" <$ told,
              "Colorizer died" <$ colored
            ]
        fail err
