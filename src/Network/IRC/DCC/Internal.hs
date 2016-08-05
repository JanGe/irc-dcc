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
import           Path                               (Abs, File, Path, Rel,
                                                     filename, fromAbsFile,
                                                     fromRelFile, parseAbsFile,
                                                     parseRelFile)
import           Prelude                            hiding (abs, unwords)

-- | Types that can be converted to CTCP commands
class ToCtcp a where
    toCtcp :: a -> CTCPByteString

-- | Types that can be parsed from CTCP commands
class FromCtcp a where
    fromCtcp :: CTCPByteString -> Either String a

parseCtcp :: Parser a -> CTCPByteString -> Either String a
parseCtcp p = parseOnly p . decodeCTCP

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

instance ToCtcp DccChat where
    toCtcp (Chat ip port) = encodeCTCP $ unwords
        [ "DCC CHAT chat"
        , socketToBS (ip, port)
        ]
    toCtcp (ChatWhiteboard ip port) = encodeCTCP $ unwords
        [ "DCC CHAT wboard"
        , socketToBS (ip, port)
        ]

instance FromCtcp DccChat where
    fromCtcp = parseCtcp $ choice
        [ do "DCC CHAT chat"
             space
             (ip, port) <- socket
             endOfInput
             return $ Chat ip port
        , do "DCC CHAT wboard"
             space
             (ip, port) <- socket
             endOfInput
             return $ ChatWhiteboard ip port
        ]

-- | Signal intent to close DCC chat connection
data DccClose
  -- | > DCC CLOSE
  = Close
  deriving (Eq, Show)

instance ToCtcp DccClose where
    toCtcp Close = encodeCTCP "DCC CLOSE"

instance FromCtcp DccClose where
    fromCtcp = parseCtcp $ do
        "DCC CLOSE"
        endOfInput
        return Close

-- | Offer DCC file transfer
data DccSend
  {-| As part of the standard DCC protocol, sent by the server

      > DCC SEND <fileName> <ip> <port> (<fileSize>)
  -}
  = Send FileName !IPv4 !PortNumber !(Maybe FileOffset)
  {-| As part of the Reverse DCC protocol, sent by the server

      > DCC SEND <fileName> <ip> 0 <fileSize> <token>
  -}
  | SendReverseServer !FileName !IPv4 !FileOffset !Token
  deriving (Eq, Show)

instance ToCtcp DccSend where
    toCtcp (Send name ip port size) = encodeCTCP $ unwords
        [ "DCC SEND"
        , fileNameToBS name
        , ipToBigEndianBS ip
        , tcpPortToBS port ]
        <> maybe "" ((" " <>) . fileOffsetToBS) size
    toCtcp (SendReverseServer name ip size t) = encodeCTCP $ unwords
        [ "DCC SEND"
        , fileNameToBS name
        , ipToBigEndianBS ip
        , "0"
        , fileOffsetToBS size
        , tokenToBS t
        ]

instance FromCtcp DccSend where
    fromCtcp = parseCtcp $ do
        "DCC SEND"
        space
        name <- fileName
        space
        ip <- ipBigEndian
        space
        choice [ do port <- tcpPort
                    size <- Just <$> (space *> fileOffset)
                        <|> return Nothing
                    endOfInput
                    return $ Send name ip port size
               , do "0"
                    space
                    size <- fileOffset
                    space
                    t <- token
                    endOfInput
                    return $ SendReverseServer name ip size t
               ]

-- | Signal intent to resume DCC file transfer at specific position
data DccResume
  {-| As part of the standard DCC protocol, sent by the client

      > DCC RESUME <fileName> <port> <position>
  -}
  = Resume !FileName !PortNumber !FileOffset
  {-| As part of the Reverse DCC protocol, sent by the client

      > DCC RESUME <fileName> 0 <position> <token>
  -}
  | ResumeReverse !FileName !FileOffset !Token
  deriving (Eq, Show)

instance ToCtcp DccResume where
    toCtcp (Resume name port pos) = encodeCTCP $ unwords
        [ "DCC RESUME"
        , fileNameToBS name
        , tcpPortToBS port
        , fileOffsetToBS pos
        ]
    toCtcp (ResumeReverse name pos t) = encodeCTCP $ unwords
        [ "DCC RESUME"
        , fileNameToBS name
        , "0"
        , fileOffsetToBS pos
        , tokenToBS t
        ]

instance FromCtcp DccResume where
    fromCtcp = parseCtcp $ do
        "DCC RESUME"
        space
        name <- fileName
        space
        choice [ do port <- tcpPort
                    space
                    pos <- fileOffset
                    endOfInput
                    return $ Resume name port pos
               , do "0"
                    space
                    pos <- fileOffset
                    space
                    t <- token
                    endOfInput
                    return $ ResumeReverse name pos t
               ]

-- | Signal acceptance to resume DCC file transfer at specific position
data DccAccept
  {-| As part of the standard DCC protocol, sent by the server

      > DCC ACCEPT <fileName> <port> <position>
  -}
  = Accept !FileName !PortNumber !FileOffset
  {-| As part of the Reverse DCC protocol, sent by the server

      > DCC ACCEPT <fileName> 0 <position> <token>
  -}
  | AcceptReverse !FileName !FileOffset !Token
  deriving (Eq, Show)

instance ToCtcp DccAccept where
    toCtcp (Accept name port pos) = encodeCTCP $ unwords
        [ "DCC ACCEPT"
        , fileNameToBS name
        , tcpPortToBS port
        , fileOffsetToBS pos
        ]
    toCtcp (AcceptReverse name pos t) = encodeCTCP $ unwords
        [ "DCC ACCEPT"
        , fileNameToBS name
        , "0"
        , fileOffsetToBS pos
        , tokenToBS t
        ]

instance FromCtcp DccAccept where
    fromCtcp = parseCtcp $ do
        "DCC ACCEPT"
        space
        name <- fileName
        space
        choice [ do port <- tcpPort
                    space
                    pos <- fileOffset
                    endOfInput
                    return $ Accept name port pos
               , do "0"
                    space
                    pos <- fileOffset
                    space
                    t <- token
                    endOfInput
                    return $ AcceptReverse name pos t
               ]

-- | Tell the server where to push to start a DCC file transfer
--   and push data to the specified location.
data DccSendReverseClient
  {-| As part of the Reverse DCC protocol, sent by the client

      > DCC SEND <fileName> <ip> <port> <fileSize> <token>
  -}
  = SendReverseClient !FileName !IPv4 !PortNumber !FileOffset !Token
  deriving (Eq, Show)

instance ToCtcp DccSendReverseClient where
  toCtcp (SendReverseClient name ip port size t) = encodeCTCP $ unwords
      [ "DCC SEND"
      , fileNameToBS name
      , ipToBigEndianBS ip
      , tcpPortToBS port
      , fileOffsetToBS size
      , tokenToBS t
      ]

instance FromCtcp DccSendReverseClient where
  fromCtcp = parseCtcp $ do
      "DCC SEND"
      space
      name <- fileName
      space
      ip <- ipBigEndian
      space
      port <- tcpPort
      space
      size <- fileOffset
      space
      t <- token
      endOfInput
      return $ SendReverseClient name ip port size t

data FileName
  = SimpleRelFn !(Path Rel File)
  | SimpleAbsFn !(Path Abs File)
  -- ^ A file name without spaces
  | QuotedRelFn !(Path Rel File)
  | QuotedAbsFn !(Path Abs File)
  -- ^ A file name that can include spaces and will be quoted when serialized
  deriving (Eq, Show)

_fileName :: FileName -> Path Rel File
_fileName (SimpleRelFn name) = name
_fileName (SimpleAbsFn name) = filename name
_fileName (QuotedRelFn name) = name
_fileName (QuotedAbsFn name) = filename name

newtype FileOffset = FileOffset { toWord :: Word64 }
  deriving (Eq, Ord, Num, Integral, Enum, Real, Bounded)

instance Show FileOffset where
  show = show . toWord

-- | An identifier for knowing which negotiation a request belongs to
newtype Token = Token ByteString
  deriving (Eq, Show)

socket :: Parser (IPv4, PortNumber)
socket =
    do i <- ipBigEndian
       space
       p <- tcpPort
       return (i, p)

socketToBS :: (IPv4, PortNumber) -> ByteString
socketToBS (i, p) = ipToBigEndianBS i <> " " <> tcpPortToBS p

fileName :: Parser FileName
fileName =
    choice [ do "\""
                name <- UTF8.toString <$> takeWhile1 (/= '"')
                "\""
                parseRelOrAbs QuotedRelFn QuotedAbsFn name
           , do name <- UTF8.toString <$> takeWhile1 (/= ' ')
                parseRelOrAbs SimpleRelFn SimpleAbsFn name
           ]
  where parseRelOrAbs rel abs n =
            maybe (fail "Could not parse file name.") return
                ( rel <$> parseRelFile n
              <|> abs <$> parseAbsFile n )

fileNameToBS :: FileName -> ByteString
fileNameToBS (SimpleRelFn name) = UTF8.fromString (fromRelFile name)
fileNameToBS (SimpleAbsFn name) = UTF8.fromString (fromAbsFile name)
fileNameToBS (QuotedRelFn name) = "\"" <> UTF8.fromString (fromRelFile name) <> "\""
fileNameToBS (QuotedAbsFn name) = "\"" <> UTF8.fromString (fromAbsFile name) <> "\""

ipBigEndian :: Parser IPv4
ipBigEndian = fromBigEndianIp <$> decimalInRange (0, 4294967295)

ipToBigEndianBS :: IPv4 -> ByteString
ipToBigEndianBS = pack . show . toBigEndianIp

tcpPort :: Parser PortNumber
tcpPort = fromInteger <$> decimalInRange (1, 65535)

tcpPortToBS :: PortNumber -> ByteString
tcpPortToBS = pack . show

fileOffset :: Parser FileOffset
fileOffset = FileOffset <$> decimal

fileOffsetToBS :: FileOffset -> ByteString
fileOffsetToBS = pack . show . toWord

token :: Parser Token
token = Token <$> takeByteString

tokenToBS :: Token -> ByteString
tokenToBS (Token t) = t

decimalInRange :: (Integer, Integer) -> Parser Integer
decimalInRange (lower, upper) = do
    num <- decimal
    when (num < lower || num > upper) $
         fail ("Failed to parse " ++ show num ++ ", not in range [" ++
               show lower ++ ", " ++ show upper ++ "].")
    return num

fromBigEndianIp :: Integer -> IPv4
fromBigEndianIp = fromHostAddress . byteSwap32 . fromIntegral

toBigEndianIp :: IPv4 -> Integer
toBigEndianIp = fromIntegral . byteSwap32 . toHostAddress
