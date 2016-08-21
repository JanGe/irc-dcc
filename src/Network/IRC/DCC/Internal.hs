{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.IRC.DCC.Internal where

import           Control.Applicative                ((<|>))
import           Control.Monad                      (when)
import           Data.Attoparsec.ByteString.Char8   (Parser, choice, decimal,
                                                     endOfInput, parseOnly,
                                                     space, takeByteString,
                                                     takeWhile1)
import           Data.Binary                        (byteSwap32)
import           Data.ByteString.Char8              (ByteString, pack, unwords)
import qualified Data.ByteString.UTF8               as UTF8 (fromString,
                                                             toString)
import           Data.IP                            (IPv4, fromHostAddress,
                                                     toHostAddress)
import           Data.Monoid                        ((<>))
import           Data.Word                          (Word64)
import           Network.IRC.CTCP                   (CTCPByteString, decodeCTCP,
                                                     encodeCTCP)
import           Network.Socket.ByteString.Extended (PortNumber)
import qualified Path                               as P (Abs, File, Path, Rel,
                                                          filename, fromAbsFile,
                                                          fromRelFile,
                                                          parseAbsFile,
                                                          parseRelFile)
import           Prelude                            hiding (abs, unwords)

-- | CTCP commands that can be parsed and encoded
class CtcpCommand a where
    toCtcp :: a -> CTCPByteString
    fromCtcp :: CTCPByteString -> Either String a

parseCtcp :: Parser a -> CTCPByteString -> Either String a
parseCtcp p = parseOnly (p <* endOfInput) . decodeCTCP

-- | Offer DCC chat session
data DccChat
  {-| Text messages exchange

      > DCC CHAT chat <ip> <port>
  -}
  = Chat !IPv4 !PortNumber
  {-| Drawing commands exchange

      > DCC CHAT wboard <ip> <port>
  -}
  | ChatWhiteboard !IPv4 !PortNumber
  deriving (Eq, Show)

instance CtcpCommand DccChat where
    toCtcp (Chat ip port) = encodeCTCP $ unwords
        [ "DCC CHAT chat"
        , socketToBS (ip, port)
        ]
    toCtcp (ChatWhiteboard ip port) = encodeCTCP $ unwords
        [ "DCC CHAT wboard"
        , socketToBS (ip, port)
        ]

    fromCtcp = parseCtcp $
      Chat
          <$> ("DCC CHAT chat" *> spaced ipBigEndian)
          <*> spaced tcpPort
      <|> ChatWhiteboard
          <$> ("DCC CHAT wboard" *> spaced ipBigEndian)
          <*> spaced tcpPort

-- | Signal intent to close DCC chat connection
data DccClose
  -- | > DCC CLOSE
  = Close
  deriving (Eq, Show)

instance CtcpCommand DccClose where
    toCtcp Close = encodeCTCP "DCC CLOSE"
    fromCtcp = parseCtcp $ Close <$ "DCC CLOSE"

-- | Offer DCC file transfer
data DccSend
  {-| As part of the standard DCC protocol, sent by the server

      > DCC SEND <path> <ip> <port> (<fileSize>)
  -}
  = Send !Path !IPv4 !PortNumber !(Maybe FileOffset)
  {-| As part of the Reverse DCC protocol, sent by the server

      > DCC SEND <path> <ip> 0 <fileSize> <token>
  -}
  | SendReverseServer !Path !IPv4 !FileOffset !Token
  deriving (Eq, Show)

instance CtcpCommand DccSend where
    toCtcp (Send name ip port size) = encodeCTCP $ unwords
        [ "DCC SEND"
        , pathToBS name
        , ipToBigEndianBS ip
        , tcpPortToBS port ]
        <> maybe "" ((" " <>) . fileOffsetToBS) size
    toCtcp (SendReverseServer name ip size t) = encodeCTCP $ unwords
        [ "DCC SEND"
        , pathToBS name
        , ipToBigEndianBS ip
        , "0"
        , fileOffsetToBS size
        , tokenToBS t
        ]

    fromCtcp = parseCtcp $
        Send
            <$> ("DCC SEND" *> spaced path)
            <*> spaced ipBigEndian
            <*> spaced tcpPort
            <*> (Just <$> spaced fileOffset
                <|> return Nothing)
        <|> SendReverseServer
            <$> ("DCC SEND" *> spaced path)
            <*> spaced ipBigEndian
            <*> (spaced "0" *> spaced fileOffset)
            <*> spaced token

-- | Signal intent to resume DCC file transfer at specific position
data DccResume
  {-| As part of the standard DCC protocol, sent by the client

      > DCC RESUME <path> <port> <position>
  -}
  = Resume !Path !PortNumber !FileOffset
  {-| As part of the Reverse DCC protocol, sent by the client

      > DCC RESUME <path> 0 <position> <token>
  -}
  | ResumeReverse !Path !FileOffset !Token
  deriving (Eq, Show)

instance CtcpCommand DccResume where
    toCtcp (Resume name port pos) = encodeCTCP $ unwords
        [ "DCC RESUME"
        , pathToBS name
        , tcpPortToBS port
        , fileOffsetToBS pos
        ]
    toCtcp (ResumeReverse name pos t) = encodeCTCP $ unwords
        [ "DCC RESUME"
        , pathToBS name
        , "0"
        , fileOffsetToBS pos
        , tokenToBS t
        ]

    fromCtcp = parseCtcp $
        Resume
            <$> ("DCC RESUME" *> spaced path)
            <*> spaced tcpPort
            <*> spaced fileOffset
        <|> ResumeReverse
            <$> ("DCC RESUME" *> spaced path)
            <*> (spaced "0" *> spaced fileOffset)
            <*> spaced token

-- | Signal acceptance to resume DCC file transfer at specific position
data DccAccept
  {-| As part of the standard DCC protocol, sent by the server

      > DCC ACCEPT <path> <port> <position>
  -}
  = Accept !Path !PortNumber !FileOffset
  {-| As part of the Reverse DCC protocol, sent by the server

      > DCC ACCEPT <path> 0 <position> <token>
  -}
  | AcceptReverse !Path !FileOffset !Token
  deriving (Eq, Show)

acceptedPosition :: DccAccept -> FileOffset
acceptedPosition (Accept _ _ pos)        = pos
acceptedPosition (AcceptReverse _ pos _) = pos

instance CtcpCommand DccAccept where
    toCtcp (Accept name port pos) = encodeCTCP $ unwords
        [ "DCC ACCEPT"
        , pathToBS name
        , tcpPortToBS port
        , fileOffsetToBS pos
        ]
    toCtcp (AcceptReverse name pos t) = encodeCTCP $ unwords
        [ "DCC ACCEPT"
        , pathToBS name
        , "0"
        , fileOffsetToBS pos
        , tokenToBS t
        ]

    fromCtcp = parseCtcp $
        Accept
            <$> ("DCC ACCEPT" *> spaced path)
            <*> spaced tcpPort
            <*> spaced fileOffset
        <|> AcceptReverse
            <$> ("DCC ACCEPT" *> spaced path)
            <*> (spaced "0" *> spaced fileOffset)
            <*> spaced token

-- | Tell the server to start a DCC file transfer and where it should send the data to.
data DccSendReverseClient
  {-| As part of the Reverse DCC protocol, sent by the client

      > DCC SEND <path> <ip> <port> <fileSize> <token>
  -}
  = SendReverseClient !Path !IPv4 !PortNumber !FileOffset !Token
  deriving (Eq, Show)

instance CtcpCommand DccSendReverseClient where
  toCtcp (SendReverseClient name ip port size t) = encodeCTCP $ unwords
      [ "DCC SEND"
      , pathToBS name
      , ipToBigEndianBS ip
      , tcpPortToBS port
      , fileOffsetToBS size
      , tokenToBS t
      ]

  fromCtcp = parseCtcp $
      SendReverseClient
          <$> ("DCC SEND" *> spaced path)
          <*> spaced ipBigEndian
          <*> spaced tcpPort
          <*> spaced fileOffset
          <*> spaced token

data PathType
  = Simple
  -- ^ A file path without spaces
  | Quoted
  -- ^ A file path that can include spaces and will be quoted when serialized
  deriving (Eq, Show)

data Path = Rel PathType (P.Path P.Rel P.File)
          | Abs PathType (P.Path P.Abs P.File)
  deriving (Eq, Show)

fromPath :: Path -> P.Path P.Rel P.File
fromPath (Rel _ name) = name
fromPath (Abs _ name) = P.filename name

path :: Parser Path
path = choice [ quoted >>= parseRelOrAbs Quoted
              , simple >>= parseRelOrAbs Simple ]
  where
    quoted = UTF8.toString <$> ("\"" *> takeWhile1 (/= '"') <* "\"")
    simple = UTF8.toString <$> takeWhile1 (/= ' ')
    parseRelOrAbs ty n =
            maybe (fail "Could not parse file name.") return
                ( Rel ty <$> P.parseRelFile n
              <|> Abs ty <$> P.parseAbsFile n )

pathToBS :: Path -> ByteString
pathToBS (Rel ty name) = wrap ty . UTF8.fromString . P.fromRelFile $ name
pathToBS (Abs ty name) = wrap ty . UTF8.fromString . P.fromAbsFile $ name

wrap :: PathType -> ByteString -> ByteString
wrap Simple p = p
wrap Quoted p = "\"" <> p <> "\""

newtype FileOffset = FileOffset { toWord :: Word64 }
  deriving (Eq, Ord, Num, Integral, Enum, Real, Bounded)

instance Show FileOffset where
  show = show . toWord

fileOffset :: Parser FileOffset
fileOffset = FileOffset <$> decimal

fileOffsetToBS :: FileOffset -> ByteString
fileOffsetToBS = pack . show . toWord

-- | An identifier for knowing which negotiation a request belongs to
newtype Token = Token ByteString
  deriving (Eq, Show)

token :: Parser Token
token = Token <$> takeByteString

tokenToBS :: Token -> ByteString
tokenToBS (Token t) = t

socket :: Parser (IPv4, PortNumber)
socket = (,) <$> ipBigEndian <* space <*> tcpPort

socketToBS :: (IPv4, PortNumber) -> ByteString
socketToBS (i, p) = ipToBigEndianBS i <> " " <> tcpPortToBS p

ipBigEndian :: Parser IPv4
ipBigEndian = fromBigEndianIp <$> decimalInRange (0, 4294967295)

ipToBigEndianBS :: IPv4 -> ByteString
ipToBigEndianBS = pack . show . toBigEndianIp

fromBigEndianIp :: Integer -> IPv4
fromBigEndianIp = fromHostAddress . byteSwap32 . fromIntegral

toBigEndianIp :: IPv4 -> Integer
toBigEndianIp = fromIntegral . byteSwap32 . toHostAddress

tcpPort :: Parser PortNumber
tcpPort = fromInteger <$> decimalInRange (1, 65535)

tcpPortToBS :: PortNumber -> ByteString
tcpPortToBS = pack . show

decimalInRange :: (Integer, Integer) -> Parser Integer
decimalInRange (lower, upper) = do
    num <- decimal
    when (num < lower || num > upper) $
       fail ( "Failed to parse " ++ show num ++ ", not in range ["
           ++ show lower ++ ", " ++ show upper ++ "]." )
    return num

spaced :: Parser a -> Parser a
spaced = (space *>)
