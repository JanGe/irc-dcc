module Network.IRC.DCC.FileTransfer
  ( acceptFile
  , resumeFile
  ) where

import           Network.IRC.DCC.Internal

import           Control.Error
import           Control.Monad              (unless)
import           Control.Monad.IO.Class     (MonadIO)
import           Data.Binary.Put            (putWord32be, runPut)
import           Data.ByteString.Char8      (ByteString, length, null)
import qualified Data.ByteString.Lazy.Char8 as Lazy (toStrict)
import           Network.Socket.Extended
import           Path                       (File, Path, Rel, fromRelFile)
import           Prelude                    hiding (length, null)
import           System.IO                  (BufferMode (NoBuffering),
                                             IOMode (WriteMode, AppendMode))
import           System.IO.Streams          (OutputStream, withFileAsOutputExt,
                                             write)

type Handle = OutputStream ByteString

acceptFile :: Offer
           -> (PortNumber -> ExceptT String IO ())
           -> (FileOffset -> IO ())
           -> ExceptT String IO ()
acceptFile (Offer tt f) = downloadFile tt (fileName f) 0

resumeFile :: AcceptResume
           -> (PortNumber -> ExceptT String IO ())
           -> (FileOffset -> IO ())
           -> ExceptT String IO ()
resumeFile (AcceptResume tt f pos) = downloadFile tt (fileName f) pos

downloadFile :: TransferType
             -> Path Rel File
             -> FileOffset
             -> (PortNumber -> ExceptT String IO ())
             -> (FileOffset -> IO ())
             -> ExceptT String IO ()
downloadFile tt fn pos onListen onChunk =
    withSocket tt onListen (\ con ->
        withFileAsOutputExt (fromRelFile fn) (fileMode pos) NoBuffering (\ f ->
            downloadToFile f onChunk pos con))
  where withSocket (Active i p) = withActiveSocket i p
        withSocket (Passive i _) = withPassiveSocket i

fileMode :: (Eq a, Num a) => a -> IOMode
fileMode 0 = WriteMode
fileMode _ = AppendMode

downloadToFile :: Handle -> (FileOffset -> IO a) -> FileOffset -> Socket
               -> IO ()
downloadToFile file onChunk size sock =
  do buffer <- recv sock $ 4096 * 1024
     unless (null buffer) $
            do let received = fromIntegral (length buffer)
               onChunk received
               let receivedTotal = size + received
               sendNumReceived sock receivedTotal
               Just buffer `write` file
               downloadToFile file onChunk receivedTotal sock

sendNumReceived :: Socket -> FileOffset -> IO ()
sendNumReceived sock num = sendAll sock $ toNetworkByteOrder num

-- TODO Simplify
toNetworkByteOrder :: FileOffset -> ByteString
toNetworkByteOrder = Lazy.toStrict . runPut . putWord32be . fromIntegral
