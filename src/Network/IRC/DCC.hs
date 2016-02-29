module Network.IRC.DCC (
  -- * Types
  -- ** DCC service
    Service(..)
  -- ** Messaging commands (DCC CHAT)
  , ChatProtocol(..)
  -- ** File Transfer commands (DCC SEND)
  , Offer(..)
  , TryResume(..)
  , AcceptResume(..)
  , OfferSink(..)
  -- *** Helper Types
  , TransferType(..)
  , FileMetadata(..)
  , Token(..)
  , FileOffset
  -- * DCC command parsing
  , runParser
  , decodeService
  , encodeService
  , decodeTryResume
  , encodeTryResume
  , decodeAcceptResume
  , encodeAcceptResume
  , decodeOfferSink
  , encodeOfferSink
  ) where

import Network.IRC.DCC.Internal
