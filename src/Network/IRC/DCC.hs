module Network.IRC.DCC (
  -- * Types
  -- ** DCC service
    CtcpCommand(..)
  , Service(..)
  -- ** Messaging commands (DCC CHAT)
  , OpenChat(..)
  , CloseChat(..)
  -- ** File Transfer commands (DCC SEND)
  , OfferFile(..)
  , TryResumeFile(..)
  , AcceptResumeFile(..)
  , OfferFileSink(..)
  -- *** Helper Types
  , TransferType(..)
  , FileMetadata(..)
  , Token(..)
  , FileOffset
  -- * DCC command parsing
  , runParser
  , decodeService
  , decodeTryResume
  , decodeAcceptResume
  , decodeOfferSink
  ) where

import Network.IRC.DCC.Internal
