{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
module Server(main) where

import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Control.Exception as E
import GHC.Generics (Generic)
import Options.Generic (ParseRecord, getRecord)
import Control.Monad (forever)
import qualified Lib
import qualified Data.Serialize  as Ser
import Data.Word(Word32)

serve :: NS.AddrInfo -> (NS.Socket -> IO a) -> IO a
serve addr go = E.bracket open NS.close go
    where
    open = E.bracketOnError (NS.openSocket addr) NS.close $ \sock -> do
        -- Set socket options here
        NS.withFdSocket sock NS.setCloseOnExecIfNeeded
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.setSocketOption sock NS.Broadcast 1
        NS.bind sock $ NS.addrAddress addr
        --NS.listen sock 4
        return sock

handler :: NS.SockAddr -> NS.Socket -> IO void
handler broadcast sock = forever $ do
    -- TODO: Can only send to 10.20.1.103, can only read from 10.20.1.255. Split it up
    len <- NSB.sendTo sock (Ser.encode $ Lib.ArtPoll $ Lib.ArtPoll_ 0 Nothing) broadcast
    print len

    -- Just in case people use mega-jumbo packets in the future
    bs <- NSB.recv sock 100000 
    case Ser.decode bs of
        Left err -> putStrLn err
        Right (cmd :: Lib.ArtCommand) -> print cmd

data Opts = Opts {hostname :: String} deriving (Generic, Show, ParseRecord)

me :: Word32
me = 0x0A1401FF

main :: IO ()
main = NS.withSocketsDo $ do
    Opts host <- getRecord "Server"
    let hints = NS.defaultHints {NS.addrSocketType = NS.Datagram, NS.addrFlags = [NS.AI_PASSIVE]}
    addrs <- NS.getAddrInfo (Just hints) (Just host) (Just "6454")
    case addrs of
        [one] -> do
            print one
            serve one (handler (NS.SockAddrInet 6454 me))
        others -> fail $ "Expected one address: " ++ show others
