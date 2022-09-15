{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.Chan as Chan
import qualified Data.ByteString as BS
import qualified Data.Serialize as Ser
import Data.Word (Word8)
import GHC.Generics (Generic)
import qualified Lib
import qualified Lib.Pixel as Px
import Options.Generic (ParseRecord, getRecord)
import qualified Server

packets :: [Lib.ArtCommand]
packets = [poll, reply, dmx]
  where
    poll = Lib.ArtPoll Lib.defaultArtPoll
    reply = Lib.ArtPollReply Lib.defaultArtPollReply
    dmx = Lib.ArtDMX $ Lib.ArtDMX_ 0 0 0 (Lib.Data packet)
    px cct = Px.CCTRGBWPx @Double @Double 0.7 cct 0 0 (Px.RGBW 0 0 0 0)
    packet = BS.concat $ map (\c -> Ser.encode $ ((fmap Px.cast $ Px.mapLo Px.cast $ px (Px.Temp c)) :: Px.CCTRGBWPx Word8 Px.W16Be)) [0, (1 / 15) .. 1]

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
