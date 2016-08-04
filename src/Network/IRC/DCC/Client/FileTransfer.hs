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
import           System.IO.Streams                  (OutputStream,
                                                     withFileAsOutputExt, write)

data TransferType = FromStart
                  | ResumeFrom FileOffset

data FileTransfer = Download { _name           :: Path Rel File
                             , _connectionType :: ConnectionType
                             , _transferType   :: TransferType
                             , _onChunk        :: FileOffset -> IO ()
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

transfer :: FileTransfer -> IO ()
transfer Download {..} =
    bracket (connect _connectionType)
            close
            (download _name _transferType _onChunk)

download :: Path Rel File
         -> TransferType
         -> (FileOffset -> IO ())
          -- ^ Callback when a chunk of data was transfered
         -> Socket
         -> IO ()
download name FromStart onChunk =
    withFileAsOutputExt (fromRelFile name) WriteMode NoBuffering .
        stream 0 onChunk
download name (ResumeFrom pos) onChunk =
    withFileAsOutputExt (fromRelFile name) AppendMode NoBuffering .
        stream pos onChunk

stream :: FileOffset
       -> (FileOffset -> IO ())
       -> Socket
       -> OutputStream ByteString
       -> IO ()
stream pos onChunk sock h = do
    buf <- recv sock (4096 * 1024)
    unless (null buf) $ do
        let len = fromIntegral $ length buf
        onChunk len
        let pos' = pos + len
        sendPosition sock pos'
        Just buf `write` h
        stream pos' onChunk sock h

sendPosition :: Socket
             -> FileOffset
             -> IO ()
sendPosition sock = sendAll sock . toNetworkByteOrder
