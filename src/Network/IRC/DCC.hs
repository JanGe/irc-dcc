{-| DCC command parsing and encoding module

    Example of parsing an offer file command:

    > runParser parseOfferFile ctcpMessage

    Example of encoding an offer file command:

    > encodeCtcp offerFile
-}
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
  , parseService
  , parseOpenChat
  , parseCloseChat
  , parseOfferFile
  , parseTryResumeFile
  , parseAcceptResumeFile
  , parseOfferFileSink
  ) where

import Network.IRC.DCC.Internal
