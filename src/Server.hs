{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Server (main) where

import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import Control.Monad (forever)
import qualified Data.Serialize as Ser
import GHC.Generics (Generic)
import qualified Lib
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import Options.Generic (ParseRecord, getRecord)
import qualified Lib.Pixel as Px
import qualified Data.ByteString as BS
import Data.Word (Word8)

serve :: NS.AddrInfo -> (NS.Socket -> IO a) -> IO a
serve addr go = E.bracket open NS.close go
  where
    open = E.bracketOnError (NS.openSocket addr) NS.close $ \sock -> do
      NS.withFdSocket sock NS.setCloseOnExecIfNeeded
      NS.setSocketOption sock NS.ReuseAddr 1
      NS.setSocketOption sock NS.Broadcast 1
      NS.bind sock $ NS.addrAddress addr
      return sock

listen :: NS.Socket -> IO void
listen sock = forever $ do
  -- Just in case people use mega-jumbo packets in the future
  bs <- NSB.recv sock 100000
  case Ser.decode bs of
    Left err -> putStrLn err
    Right (cmd :: Lib.ArtCommand) -> print cmd

tell :: NS.SockAddr -> NS.Socket -> IO ()
tell broadcast sock = do
  let px cct = Px.CCTRGBWPx @Double @Double 0.7 cct 0 0 (Px.RGBW 0 0 0 0)
      packet = BS.concat $ map (\c -> Ser.encode $ ((fmap Px.cast $ Px.mapLo Px.cast $ px (Px.Temp c)) :: Px.CCTRGBWPx Word8 Px.W16Be)) [0, (1/15) .. 1]
  _len <- NSB.sendTo sock (Ser.encode $ Lib.ArtPoll $ Lib.defaultArtPoll) broadcast
  _len <- NSB.sendTo sock (Ser.encode $ Lib.ArtDMX $ Lib.ArtDMX_ 0 0 0 (Lib.Data packet)) broadcast
  return ()

getAddr :: String -> IO NS.AddrInfo
getAddr name = do
  let hints =
        NS.defaultHints
          { NS.addrSocketType = NS.Datagram,
            NS.addrFlags = [NS.AI_PASSIVE]
          }
  addrs <- NS.getAddrInfo (Just hints) (Just name) (Just "6454")
  case addrs of
    [one] -> return one
    others -> fail $ "Expected precisely one network address, but found these: " ++ show others

data Opts = Opts {broadcastAddr :: String, localAddr :: String} deriving (Generic, Show, ParseRecord)

main :: IO ()
main = NS.withSocketsDo $ do
  Opts broadcast_ local_ <- getRecord "Server"
  broadcast <- getAddr broadcast_
  local <- getAddr local_
  let listener = serve broadcast listen
      teller = serve local (tell $ NS.addrAddress broadcast)
  Async.withAsync listener $ \listened -> do
    Async.withAsync teller $ \_told -> do
      (_, err) <-
        Async.waitAny
          [ 
            "Listener died" <$ listened
          ]
      fail err
