module Network.Socket.ByteString.Extended
  ( module Network.Socket
  , module Network.Socket.ByteString
  , withActiveSocket
  , withPassiveSocket
  ) where

import           Control.Error
import           Control.Monad.IO.Class    (liftIO)
import           Data.IP                   (IPv4, fromHostAddress,
                                            toHostAddress)
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString
import           System.Timeout

withActiveSocket :: IPv4 -> PortNumber
                 -> (PortNumber -> ExceptT String IO ())
                 -> (Socket -> IO ())
                 -> ExceptT String IO ()
withActiveSocket i p onListen onConnected = do
    liftIO $ return withSocketsDo
    sock <- liftIO $ socket AF_INET Stream defaultProtocol
    onListen p
    liftIO $ connect sock (SockAddrInet p (toHostAddress i))
    liftIO $ onConnected sock
    liftIO $ sClose sock

withPassiveSocket :: IPv4
                  -> (PortNumber -> ExceptT String IO ())
                  -> (Socket -> IO ())
                  -> ExceptT String IO ()
withPassiveSocket i onListen onConnected = do
    liftIO $ return withSocketsDo
    sock <- liftIO $ socket AF_INET Stream defaultProtocol
    liftIO $ bind sock (SockAddrInet aNY_PORT iNADDR_ANY)
    liftIO $ listen sock 1
    p <- liftIO $ socketPort sock
    onListen p
    accepted <- liftIO $ timeout 10000000 $ accept sock
    case accepted of
      Just (con, SockAddrInet _ client)
        | client == toHostAddress i -> liftIO $ do onConnected con
                                                   sClose con
        | otherwise -> do liftIO $ sClose con
                          throwE ( "Expected connection from host "
                                ++ show (fromHostAddress client)
                                ++ ", not from " ++ show i ++". Aborting…\n" )

      _ -> throwE ( "Timeout when waiting for other party to connect on port "
                 ++ show p ++ "…\n")
    liftIO $ sClose sock
