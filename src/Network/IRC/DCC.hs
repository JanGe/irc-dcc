module Network.IRC.DCC (
  -- * Types
  -- ** DCC
    Service(..)
  -- ** DCC CHAT
  , ChatProtocol(..)
  -- ** DCC SEND
  , Offer(..)
  , TryResume(..)
  , AcceptResume(..)
  , OfferSink(..)
  -- *** Helper Types
  , TransferType(..)
  , FileMetadata(..)
  , Token(..)
  , FileOffset
  -- ** CTCP message parsing
  , runParser
  , decodeOffer
  , encodeOffer
  , decodeTryResume
  , encodeTryResume
  , decodeAcceptResume
  , encodeAcceptResume
  , decodeOfferSink
  , encodeOfferSink
  , fileMetadata
  , fileName
  , fileSize
  ) where

import Network.IRC.DCC.Internal
