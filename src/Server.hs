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
import qualified Control.Concurrent.Async as Async

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

listen ::  NS.Socket -> IO void
listen sock = forever $ do
    -- Just in case people use mega-jumbo packets in the future
    bs <- NSB.recv sock 100000 
    case Ser.decode bs of
        Left err -> putStrLn err
        Right (cmd :: Lib.ArtCommand) -> print cmd

tell :: NS.SockAddr -> NS.Socket -> IO ()
tell broadcast sock = do
    len <- NSB.sendTo sock (Ser.encode $ Lib.ArtPoll $ Lib.ArtPoll_ 0 0 Nothing) broadcast
    print len


data Opts = Opts {broadcastAddr :: String, localAddr :: String} deriving (Generic, Show, ParseRecord)


getAddr :: String -> IO NS.AddrInfo
getAddr name = do
    let hints = NS.defaultHints {NS.addrSocketType = NS.Datagram, NS.addrFlags = [NS.AI_PASSIVE]}
    addrs <- NS.getAddrInfo (Just hints) (Just name) (Just "6454")
    case addrs of
        [one] -> return one
        others -> fail $ "Expected one address: " ++ show others

main :: IO ()
main = NS.withSocketsDo $ do
    Opts broadcast_ local_ <- getRecord "Server"
    broadcast <- getAddr broadcast_
    local <- getAddr local_
    let listener = serve broadcast listen
        teller = serve local (tell $ NS.addrAddress broadcast)
    Async.withAsync listener $ \listened ->
        Async.withAsync teller $ \told ->
            Async.wait told >> Async.wait listened
    
