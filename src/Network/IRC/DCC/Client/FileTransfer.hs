{-# LANGUAGE RecordWildCards #-}

{-| Functions for receiving files.

    Each chunk is acknowledged by sending the total sum of bytes received for a
    file. See the
    <http://www.irchelp.org/irchelp/rfc/ctcpspec.html CTCP specification>.
-}
module Network.IRC.DCC.Client.FileTransfer
    ( FileTransfer(..)
    , ConnectionType(..)
    , TransferType(..)
    , acceptFile
    , resumeFile
    , transfer
    ) where

import           Network.IRC.DCC.Internal

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
import           Path                               (File, Path, Rel,
                                                     fromRelFile)
import           Prelude                            hiding (length, null)
import           System.IO                          (BufferMode (NoBuffering), IOMode (AppendMode, WriteMode))
import           System.IO.Streams                  (OutputStream, write)
import           System.IO.Streams.Lifted           (withFileAsOutputExt)

data TransferType = FromStart
                  | ResumeFrom !FileOffset

data FileTransfer m = Download { _name           :: !(Path Rel File)
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
acceptFile (Send name ip port _) onListen onChunk = lift $
    transfer Download { _name           = _fileName name
                      , _connectionType = Active ip port (onListen port)
                      , _transferType   = FromStart
                      , _onChunk        = onChunk
                      }
acceptFile (SendReverseServer name ip _ _) onListen onChunk = do
    port <- ask
    lift $ transfer Download { _name           = _fileName name
                             , _connectionType = Passive ip port onListen
                             , _transferType   = FromStart
                             , _onChunk        = onChunk
                             }

-- | Accept a DCC file offer for a partially downloaded file
resumeFile :: DccSend
           -> DccAccept
           -> (PortNumber -> IO ())
           -- ^ Callback when socket is ready
           -> (FileOffset -> IO ())
           -- ^ Callback when a chunk of data was transferred
           -> ReaderT (Maybe PortNumber) IO ()
resumeFile (Send name ip port _) (Accept _ _ pos) onListen onChunk = lift $
    transfer Download { _name           = _fileName name
                      , _connectionType = Active ip port (onListen port)
                      , _transferType   = ResumeFrom pos
                      , _onChunk        = onChunk
                      }
resumeFile (SendReverseServer name ip _ _) (AcceptReverse _ pos _) onListen onChunk = do
    port <- ask
    lift $ transfer Download { _name           = _fileName name
                             , _connectionType = Passive ip port onListen
                             , _transferType   = ResumeFrom pos
                             , _onChunk        = onChunk
                             }
resumeFile _ _ _ _ =
    fail "You mixed the DCC and Reverse DCC workflow when calling 'resumeFile'."

transfer :: (MonadMask m, MonadIO m) => FileTransfer m -> m ()
transfer Download {..} =
    bracket (connect _connectionType)
            (liftIO . close)
            (download _name _transferType _onChunk)

download :: (MonadMask m, MonadIO m)
         => Path Rel File
         -> TransferType
         -> (FileOffset -> m ())
          -- ^ Callback when a chunk of data was transfered
         -> Socket
         -> m ()
download name FromStart onChunk =
    withFileAsOutputExt (fromRelFile name) WriteMode NoBuffering .
        stream 0 onChunk
download name (ResumeFrom pos) onChunk =
    withFileAsOutputExt (fromRelFile name) AppendMode NoBuffering .
        stream pos onChunk

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
