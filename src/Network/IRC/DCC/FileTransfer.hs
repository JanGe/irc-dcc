{-| Functions for receiving files.

    Each chunk is acknowledged by sending the total sum of bytes received for a
    file. See the
    <http://www.irchelp.org/irchelp/rfc/ctcpspec.html CTCP specification>.
-}
module Network.IRC.DCC.FileTransfer
  ( acceptFile
  , resumeFile
  ) where

import           Network.IRC.DCC.Internal

import           Control.Error
import           Control.Monad                      (unless)
import           Control.Monad.Trans.Class          (lift)
import           Control.Monad.Trans.Reader         (ReaderT, ask)
import           Data.ByteString.Char8              (ByteString, length, null)
import           Network.Socket.ByteString.Extended
import           Path                               (File, Path, Rel,
                                                     fromRelFile)
import           Prelude                            hiding (length, null)
import           System.IO                          (BufferMode (NoBuffering), IOMode (AppendMode, WriteMode))
import           System.IO.Streams                  (OutputStream,
                                                     withFileAsOutputExt, write)

-- | Accept a DCC file offer
acceptFile :: Integral a
           => OfferFile
           -> (PortNumber -> ExceptT String IO ())
           -- ^ Callback when socket is ready
           -> (a -> IO ())
           -- ^ Callback when a chunk of data was transfered
           -> ReaderT (Maybe PortNumber) (ExceptT String IO) ()
acceptFile (OfferFile tt f) =
    download (fileName f) WriteMode 0 tt

-- | Accept a DCC file offer for a partially downloaded file
resumeFile :: Integral a
           => AcceptResumeFile
           -> (PortNumber -> ExceptT String IO ())
           -- ^ Callback when socket is ready
           -> (a -> IO ())
           -- ^ Callback when a chunk of data was transfered
           -> ReaderT (Maybe PortNumber) (ExceptT String IO) ()
resumeFile (AcceptResumeFile tt f pos) =
    download (fileName f) AppendMode (fromIntegral pos) tt

download :: Integral a
         => Path Rel File
         -> IOMode
         -> a
         -> TransferType
         -> (PortNumber -> ExceptT String IO ())
           -- ^ Callback when socket is ready
         -> (a -> IO ())
           -- ^ Callback when a chunk of data was transfered
         -> ReaderT (Maybe PortNumber) (ExceptT String IO) ()
download fn mode pos tt onListen onChunk = do
    localPort <- ask
    lift $
        withSocket tt localPort onListen $
            withFileAsOutputExt (fromRelFile fn) mode NoBuffering .
                stream pos onChunk
  where withSocket (Active i p) _ = withActiveSocket (Sink i p)
        withSocket (Passive i _) p = withPassiveSocket (Source i p)

stream :: Integral a
       => a
       -> (a -> IO ())
       -> Socket
       -> OutputStream ByteString
       -> IO ()
stream pos onChunk sock h =
  do buf <- recv sock (4096 * 1024)
     unless (null buf) $
            do let l = fromIntegral (length buf)
               onChunk l
               let pos' = pos + l
               sendPosition sock pos'
               Just buf `write` h
               stream pos' onChunk sock h

sendPosition :: Integral a => Socket -> a -> IO ()
sendPosition sock = sendAll sock . toNetworkByteOrder
