{-# OPTIONS_GHC -Wno-partial-fields #-}
module Main (main) where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan as Chan
import Control.Concurrent (threadDelay)
import Data.Word (Word16, Word8)
import GHC.Generics (Generic)
import qualified Artnet
import qualified Artnet.Data as Data
import qualified Artnet.Pixel as Px
import Options.Generic (ParseRecord, getRecord)
import qualified Server

packets :: [Artnet.ArtCommand]
packets = [poll, reply]
  where
    poll = Artnet.ArtPoll Artnet.defaultArtPoll
    reply = Artnet.ArtPollReply Artnet.defaultArtPollReply
   
data Opts = ServerLoop {broadcastAddr :: String, localAddr :: String} 
          | Uniform {broadcastAddr :: String, localAddr :: String, power :: Double, cct :: Double} 
          deriving (Generic, Show, ParseRecord)

colorizer :: (Artnet.ArtCommand -> IO ()) -> IO void
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
            cmd = Artnet.ArtDMX $ Artnet.ArtDMX_ 0 0 0 packet
        send cmd
        threadDelay 10_000
        go colors
    -- Create a CCT/RGBW pixel with Double [0,1] values
    px cct = Px.CCTRGBWPx @Double @Double 0.7 (Px.Temp cct) 0 0 (Px.RGBW 0 0 0 0)
    -- 10x oversampled
    temps = let ts = [0, (1 / (15 * 10)) .. 1] in ts ++ reverse ts

    

serverLoop :: String -> String -> IO () 
serverLoop broadcastAddr localAddr = do
    chan <- Chan.newChan
    (listener, teller) <- Server.mkServers (either putStrLn print) (Chan.readChan chan) broadcastAddr localAddr
    mapM_ (Chan.writeChan chan . Right) packets
    Async.withAsync (colorizer (Chan.writeChan chan . Right)) $ \colored -> 
      Async.withAsync listener $ \listened -> do
        Async.withAsync teller $ \told -> do
          (_, err) <-
            Async.waitAny
              [ "Listener died" <$ listened,
                "Teller died" <$ told,
                "Colorizer died" <$ colored
              ]
          fail err

uniform :: String -> String -> Double -> Double -> IO ()
uniform broadcastAddr localAddr power cct = do
    chan <- Chan.newChan
    (_listener, teller) <- Server.mkServers (either putStrLn print) (Chan.readChan chan) broadcastAddr localAddr
    let  
        px :: Px.CCTRGBWPx Word8 Word16
        px = Px.rounded $ Px.CCTRGBWPx @Double @Double (Px.Dimmer power) (Px.Temp cct) 0 0 (Px.RGBW 0 0 0 0)
        zeros = repeat px
        tube1 = take 16 zeros
        tube2 = tube1
        packet = case Data.finalize <$> (Data.add 0 tube1 Data.fresh >>= Data.add 256 tube2) of
            Nothing -> error "Couldn't fit data in DMX packet"
            Just pkt -> pkt
        cmd = Artnet.ArtDMX $ Artnet.ArtDMX_ 0 0 0 packet
    Chan.writeChan chan (Right cmd)
    Chan.writeChan chan (Left ())
    teller
    

main :: IO ()
main = getRecord "Server" >>= \case
    ServerLoop {..} -> serverLoop broadcastAddr localAddr
    Uniform {..} -> uniform broadcastAddr localAddr power cct
      
