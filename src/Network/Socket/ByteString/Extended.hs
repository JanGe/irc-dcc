-- | Common functions simplyfing the use of "Network.Socket.ByteString"
module Network.Socket.ByteString.Extended
  ( ConnectionType(..)
  , Socket(..)
  , S.PortNumber
  , connect
  , close
  , recv
  , sendAll
  , toNetworkByteOrder
  ) where

import           Data.Binary.Put            (putWord32be, runPut)
import           Data.ByteString.Char8      (ByteString)
import qualified Data.ByteString.Lazy.Char8 as Lazy (toStrict)
import           Data.IP                    (IPv4, toHostAddress)
import           Data.Maybe                 (fromMaybe)
import qualified Network.Socket             as S hiding (recv, recvFrom, send,
                                                  sendTo)
import qualified Network.Socket.ByteString  as S

data ConnectionType
  = Active !IPv4 !S.PortNumber (IO ())
  -- ^ With callback when socket is ready
  | Passive !IPv4 !(Maybe S.PortNumber) (S.PortNumber -> IO ())
  -- ^ With callback when socket is ready

data Socket
  = ActiveSocket !S.Socket
  | PassiveSocket !S.Socket !S.PortNumber

_socket :: Socket -> S.Socket
_socket (ActiveSocket sock)    = sock
_socket (PassiveSocket sock _) = sock

connect :: ConnectionType -> IO Socket
connect (Active ip port onListen) = do
    sock <- connectTo ip port
    onListen
    waitForConnection ip sock
connect (Passive ip maybePort onListen) = do
    sock@(PassiveSocket _ port) <- listenOn maybePort
    onListen port
    waitForConnection ip sock

close :: Socket -> IO ()
close = S.close . _socket

recv :: Socket -> Int -> IO ByteString
recv = S.recv . _socket

sendAll :: Socket -> ByteString -> IO ()
sendAll = S.sendAll . _socket

connectTo :: IPv4 -> S.PortNumber -> IO Socket
connectTo ip port = S.withSocketsDo $ do
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.connect sock (S.SockAddrInet port (toHostAddress ip))
    return $ ActiveSocket sock

listenOn :: Maybe S.PortNumber -> IO Socket
listenOn port = S.withSocketsDo $ do
    sock  <- openListenSocket (fromMaybe S.aNY_PORT port)
    port' <- S.socketPort sock
    return $ PassiveSocket sock port'

waitForConnection :: IPv4 -> Socket -> IO Socket
waitForConnection _  sock@(ActiveSocket _)     = return sock -- Already connected
waitForConnection ip (PassiveSocket sock port) = do
    accepted <- S.accept sock
    case accepted of
      (con, S.SockAddrInet _ client)
        | client == toHostAddress ip -> return $ PassiveSocket con port
      _ -> fail ( "Connection did not come from " ++ show ip ++ " as expected." )

openListenSocket :: S.PortNumber -> IO S.Socket
openListenSocket p = do
    sock <- S.socket S.AF_INET S.Stream S.defaultProtocol
    S.bind sock (S.SockAddrInet p S.iNADDR_ANY)
    S.listen sock 1
    return sock

-- | Converts numbers to a '32bit unsigned int' in network byte order.
toNetworkByteOrder :: Integral a => a -> ByteString
toNetworkByteOrder = Lazy.toStrict . runPut . putWord32be . fromIntegral
