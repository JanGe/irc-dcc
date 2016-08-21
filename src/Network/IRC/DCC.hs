{-| DCC command parsing and encoding module.

    Use the 'CtcpCommand' type class to convert between 'CTCPByteString's
    and typed values.

    Try converting a 'CTCPByteString' to a 'DccSend' value:

    > fromCtcp ctcpMessage :: Either String DccSend

    Encoding a 'DccSend' value to a 'CTCPByteString':

    > toCtcp (Send fileName ip port (Just fileSize))
-}
module Network.IRC.DCC (
  -- * DCC command parsing and encoding
    CtcpCommand(..)
  -- * DCC command types
  -- ** Messaging commands (DCC CHAT)
  , DccChat(..)
  , DccClose(..)
  -- ** File Transfer commands (DCC SEND)
  , DccSend(..)
  , DccResume(..)
  , DccAccept(..)
  , acceptedPosition
  , DccSendReverseClient(..)
  -- *** Constructors from other commands
  , resumeFromSend
  -- *** Protocol variant checks
  , matchesSend
  -- ** Helper Types
  , Path(..)
  , fromPath
  , PathType(..)
  , FileOffset
  , Token(..)
  ) where

import           Network.IRC.DCC.Internal

-- | Try resuming a file offer
resumeFromSend :: DccSend -> FileOffset -> DccResume
resumeFromSend (Send path' _ port _) pos =
    Resume path' port pos
resumeFromSend (SendReverseServer path' _ _ token') pos =
    ResumeReverse path' pos token'

-- | Check if a 'DccSend' and a 'DccAccept' command are part of the same negotiation.
matchesSend :: DccAccept -> DccSend -> Bool
matchesSend (Accept pathA portA _) (Send pathS _ portS _) =
    pathS == pathA && portS == portA
matchesSend (AcceptReverse pathA _ tokenA) (SendReverseServer pathS _ _ tokenS) =
    pathS == pathA && tokenS == tokenA
matchesSend _ _ = False
