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

serve :: NS.AddrInfo -> (NS.Socket -> IO a) -> IO a
serve addr go = E.bracket open NS.close go
    where
    open = E.bracketOnError (NS.openSocket addr) NS.close $ \sock -> do
        -- Set socket options here
        NS.setSocketOption sock NS.ReuseAddr 1
        NS.setSocketOption sock NS.Broadcast 1
        return sock

handler :: NS.Socket -> IO void
handler sock = forever $ do
    -- Just in case people use mega-jumbo packets in the future
    bs <- NSB.recv sock 100000 
    case Ser.decode bs of
        Left err -> putStrLn err
        Right (cmd :: Lib.ArtCommand) -> print cmd

data Opts = Opts {hostname :: String} deriving (Generic, Show, ParseRecord)

main :: IO ()
main = do
    Opts host <- getRecord "Server"
    let hints = NS.defaultHints {NS.addrSocketType = NS.Datagram}
    addrs <- NS.getAddrInfo (Just hints) (Just host) (Just "6454")
    case addrs of
        [one] -> serve one handler 
        others -> fail $ "Expected one address: " ++ show others
