{-# LANGUAGE RecordWildCards #-}

{-| Functions for receiving files.

    Each chunk is acknowledged by sending the total sum of bytes received for a
    file. See the
    <http://www.irchelp.org/irchelp/rfc/ctcpspec.html CTCP specification>.
-}
module Network.IRC.DCC.Client.FileTransfer (
    -- * Start/resume DCC file transfer
      acceptFile
    , resumeFile
    -- * Custom DCC file transfer
    , FileTransfer(..)
    , ConnectionType(..)
    , TransferType(..)
    , transfer
    ) where

import           Network.IRC.DCC

import           Control.Exception.Safe
import           Control.Monad                      (unless)
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Control.Monad.Trans.Class          (lift)
import           Control.Monad.Trans.Reader         (ReaderT, ask)
import           Data.ByteString.Char8              (ByteString, length, null)
import           Network.Socket.ByteString.Extended (ConnectionType (..),
                                                     PortNumber, Socket, close,
                                                     connect, recv, sendAll,
                                                     toNetworkByteOrder)
import qualified Path                               as P (File, Path, Rel,
                                                          fromRelFile)
import           Prelude                            hiding (length, null)
import           System.IO                          (BufferMode (NoBuffering), IOMode (AppendMode, WriteMode))
import           System.IO.Streams                  (OutputStream, write)
import           System.IO.Streams.Lifted           (withFileAsOutputExt)

data TransferType = FromStart
                  | ResumeFrom !FileOffset

-- | A description of a file transfer.
--   You can specify a callback that will be called with the number of bytes
--   transfered for each chunk.
data FileTransfer m = Download { _fileName       :: !(P.Path P.Rel P.File)
                               , _connectionType :: !(ConnectionType m)
                               , _transferType   :: !TransferType
                               , _onChunk        :: FileOffset -> m ()
                               }

-- | Accept a DCC file offer
acceptFile :: DccSend
           -> (PortNumber -> IO ())
           -- ^ Callback when socket is ready
           -> (FileOffset -> IO ())
           -- ^ Callback when a chunk of data was transfered
           -> ReaderT (Maybe PortNumber) IO ()
acceptFile = download FromStart

-- | Accept a DCC file offer for a partially downloaded file
resumeFile :: DccSend
           -> DccAccept
           -> (PortNumber -> IO ())
           -- ^ Callback when socket is ready
           -> (FileOffset -> IO ())
           -- ^ Callback when a chunk of data was transferred
           -> ReaderT (Maybe PortNumber) IO ()
resumeFile offer accept
    | accept `matchesSend` offer =
          download (ResumeFrom pos) offer
    | otherwise = fail "You mixed the DCC and Reverse DCC workflow when calling 'resumeFile'."
  where
    pos = acceptedPosition accept

download :: TransferType
         -> DccSend
         -> (PortNumber -> IO ())
         -> (FileOffset -> IO ())
         -> ReaderT (Maybe PortNumber) IO ()
download tt (Send path ip port _) onListen onChunk =
    lift $ transfer Download { _fileName       = fromPath path
                             , _connectionType = Active ip port (onListen port)
                             , _transferType   = tt
                             , _onChunk        = onChunk
                             }
download tt (SendReverseServer path ip _ _) onListen onChunk = do
    port <- ask
    lift $ transfer Download { _fileName       = fromPath path
                             , _connectionType = Passive ip port onListen
                             , _transferType   = tt
                             , _onChunk        = onChunk
                             }

transfer :: (MonadMask m, MonadIO m) => FileTransfer m -> m ()
transfer Download {..} =
    bracket (connect _connectionType)
            (liftIO . close)
            (streamToFile _fileName _transferType _onChunk)

streamToFile :: (MonadMask m, MonadIO m)
             => P.Path P.Rel P.File
             -> TransferType
             -> (FileOffset -> m ())
              -- ^ Callback when a chunk of data was transfered
             -> Socket
             -> m ()
streamToFile name tt onChunk =
    withFileAsOutputExt (P.fromRelFile name) (mode tt) NoBuffering .
        stream (pos tt) onChunk
  where
    mode FromStart      = WriteMode
    mode (ResumeFrom _) = AppendMode
    pos FromStart       = 0
    pos (ResumeFrom p)  = p

stream :: (MonadMask m, MonadIO m)
       => FileOffset
       -> (FileOffset -> m ())
       -> Socket
       -> OutputStream ByteString
       -> m ()
stream pos onChunk sock h = do
    buf <- liftIO $ recv sock (4096 * 1024)
    unless (null buf) $ do
        let len = fromIntegral $ length buf
        onChunk len
        let pos' = pos + len
        sendPosition sock pos'
        liftIO $ Just buf `write` h
        stream pos' onChunk sock h

sendPosition :: (MonadMask m, MonadIO m)
             => Socket
             -> FileOffset
             -> m ()
sendPosition sock = liftIO . sendAll sock . toNetworkByteOrder
