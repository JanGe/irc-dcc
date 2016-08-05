-- | Some functions generalized from "System.IO.Streams"
module System.IO.Streams.Lifted
  ( withBinaryFile
  , withFileAsOutputExt
  ) where

import           Control.Exception.Safe (MonadMask, bracket)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.ByteString.Char8  (ByteString)
import           System.IO              (BufferMode, Handle, IOMode, hClose,
                                         hSetBuffering, openBinaryFile)
import           System.IO.Streams      (OutputStream, handleToOutputStream)

withBinaryFile :: (MonadMask m, MonadIO m)
                => FilePath
                -> IOMode
                -> (Handle -> m a)
                -> m a
withBinaryFile name mode =
    bracket (liftIO $ openBinaryFile name mode)
            (liftIO . hClose)

withFileAsOutputExt :: (MonadMask m, MonadIO m)
                     => FilePath
                     -> IOMode
                     -> BufferMode
                     -> (OutputStream ByteString -> m a)
                     -> m a
withFileAsOutputExt name mode buffermode f =
    withBinaryFile name mode $ \h -> do
        liftIO $ hSetBuffering h buffermode
        liftIO (handleToOutputStream h) >>= f
