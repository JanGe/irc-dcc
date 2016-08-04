{-| DCC command parsing and encoding module.

    Use the 'ToCtcp' and 'FromCtcp' type classes to convert between 'CTCPByteString's
    and typed values.

    Try converting a 'CTCPByteString' to a 'DccSend' value:

    > (fromCtcp ctcpMessage) :: Either String DccSend

    Encoding a 'DccSend' value to a 'CTCPByteString':

    > toCtcp (DccSend fileName ip port (Just fileSize))
-}
module Network.IRC.DCC (
  -- * Types
  -- ** DCC command conversion
    ToCtcp(..)
  , FromCtcp(..)
  -- ** Messaging commands (DCC CHAT)
  , DccChat(..)
  , DccClose(..)
  -- ** File Transfer commands (DCC SEND)
  , DccSend(..)
  , DccResume(..)
  , DccAccept(..)
  -- *** Helper Types
  , FileName(..)
  , FileOffset
  , Token(..)
  ) where

import           Network.IRC.DCC.Internal
