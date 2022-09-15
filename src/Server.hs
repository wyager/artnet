module Server (mkServers) where

import qualified Control.Exception as E
import Control.Monad (forever)
import qualified Data.Serialize as Ser
import qualified Lib
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB

serve :: NS.AddrInfo -> (NS.Socket -> IO a) -> IO a
serve addr go = E.bracket open NS.close go
  where
    open = E.bracketOnError (NS.openSocket addr) NS.close $ \sock -> do
      NS.withFdSocket sock NS.setCloseOnExecIfNeeded
      NS.setSocketOption sock NS.ReuseAddr 1
      NS.setSocketOption sock NS.Broadcast 1
      NS.bind sock $ NS.addrAddress addr
      return sock

listen :: (Either String Lib.ArtCommand -> IO ()) -> NS.Socket -> IO void
listen report sock = forever $ do
  -- Just in case people use mega-jumbo packets in the future
  bs <- NSB.recv sock 100000
  report (Ser.decode bs)

tell :: IO Lib.ArtCommand -> NS.SockAddr -> NS.Socket -> IO void
tell next broadcast sock = forever $ do
  cmd <- next
  NSB.sendTo sock (Ser.encode cmd) broadcast

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

mkServers :: (Either String Lib.ArtCommand -> IO ()) -> IO Lib.ArtCommand -> String -> String -> IO (IO void, IO void)
mkServers receive send broadcastAddr localAddr = do
  broadcast <- getAddr broadcastAddr
  local <- getAddr localAddr
  let listener = serve broadcast $ listen receive
      teller = serve local $ tell send (NS.addrAddress broadcast)
  return (listener, teller)
