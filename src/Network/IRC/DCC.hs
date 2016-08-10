{-| DCC command parsing and encoding module.

    Use the 'ToCtcp' and 'FromCtcp' type classes to convert between 'CTCPByteString's
    and typed values.

    Try converting a 'CTCPByteString' to a 'DccSend' value:

    > (fromCtcp ctcpMessage) :: Either String DccSend

    Encoding a 'DccSend' value to a 'CTCPByteString':

    > toCtcp (DccSend fileName ip port (Just fileSize))
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
  , resumeFromSend
  , DccAccept(..)
  , acceptedPosition
  , matchesSend
  , DccSendReverseClient(..)
  -- ** Helper Types
  , Path(..)
  , fromPath
  , PathType(..)
  , FileOffset
  , Token(..)
  ) where

import           Network.IRC.DCC.Internal

resumeFromSend :: DccSend -> FileOffset -> DccResume
resumeFromSend (Send path' _ port _) pos =
    Resume path' port pos
resumeFromSend (SendReverseServer path' _ _ token') pos =
    ResumeReverse path' pos token'

acceptedPosition :: DccAccept -> FileOffset
acceptedPosition (Accept _ _ pos)        = pos
acceptedPosition (AcceptReverse _ pos _) = pos

matchesSend :: DccAccept -> DccSend -> Bool
matchesSend (Accept pathA portA _) (Send pathS _ portS _) =
    pathS == pathA && portS == portA
matchesSend (AcceptReverse pathA _ tokenA) (SendReverseServer pathS _ _ tokenS) =
    pathS == pathA && tokenS == tokenA
matchesSend _ _ = False
