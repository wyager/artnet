{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan as Chan
import Data.Word (Word8)
import GHC.Generics (Generic)
import qualified Lib
import qualified Lib.Pixel as Px
import Options.Generic (ParseRecord, getRecord)
import qualified Server
import qualified Lib.Data as Data

packets :: [Lib.ArtCommand]
packets = [poll, reply, dmx]
  where
    poll = Lib.ArtPoll Lib.defaultArtPoll
    reply = Lib.ArtPollReply Lib.defaultArtPollReply
    dmx = Lib.ArtDMX $ Lib.ArtDMX_ 0 0 0 packet
    px cct = Px.CCTRGBWPx @Double @Double 0.7 (Px.Temp cct) 0 0 (Px.RGBW 0 0 0 0)
    tube1 :: [Px.CCTRGBWPx Word8 Px.W16Be] = (Px.rounded . px) <$> [0, (1 / 15) .. 1]
    tube2 = reverse tube1
    packet = case Data.finalize <$> (Data.add 1 tube1 Data.fresh >>= Data.add 257 tube2) of
        Nothing -> error "Couldn't fit data in DMX packet"
        Just pkt -> pkt

data Opts = Opts {broadcastAddr :: String, localAddr :: String} deriving (Generic, Show, ParseRecord)

main :: IO ()
main = do
  Opts {..} <- getRecord "Server"
  chan <- Chan.newChan
  (listener, teller) <- Server.mkServers (either putStrLn print) (Chan.readChan chan) broadcastAddr localAddr
  mapM_ (Chan.writeChan chan) packets
  Async.withAsync listener $ \listened -> do
    Async.withAsync teller $ \told -> do
      (_, err) <-
        Async.waitAny
          [ "Listener died" <$ listened,
            "Teller died" <$ told
          ]
      fail err
