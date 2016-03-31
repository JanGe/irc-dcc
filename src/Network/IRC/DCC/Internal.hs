{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Network.IRC.DCC.Internal where

import           Control.Applicative
import           Control.Monad                    (when)
import           Data.Attoparsec.ByteString.Char8
import           Data.Binary                      (byteSwap32)
import           Data.ByteString.Char8            (ByteString, pack)
import qualified Data.ByteString.UTF8             as UTF8 (toString)
import           Data.IP                          (IPv4, fromHostAddress,
                                                   toHostAddress)
import           Data.Monoid                      ((<>))
import           Data.Word                        (Word64)
import           Network.IRC.CTCP                 (CTCPByteString, decodeCTCP,
                                                   encodeCTCP)
import           Network.Socket                   (PortNumber)
import           Path                             (File, Path, Rel, filename,
                                                   fromRelFile, parseAbsFile,
                                                   parseRelFile)

-- | Type of DCC service offered
data Service
  -- | Offer chat session
  = Messaging OpenChat
  -- | Offer file transfer
  | FileTransfer OfferFile
  deriving (Eq, Show)

-- | Type of DCC chat to open
data OpenChat
  {-| Text messages exchange

      > DCC CHAT chat <ip> <port>
  -}
  = Chat IPv4 PortNumber
  {-| Drawing commands exchange

      > DCC CHAT wboard <ip> <port>
  -}
  | Whiteboard IPv4 PortNumber
  deriving (Eq, Show)

-- | Signal intent to close DCC chat connection
data CloseChat
  -- | > DCC CLOSE
  = CloseChat
  deriving (Eq, Show)

-- | DCC file transfer instructions
data OfferFile
  {-| DCC:

      > DCC SEND <fileName> <ip> <port> (<fileSize>)

      Reverse DCC:

      > DCC SEND <fileName> <ip> 0 <fileSize> <token>
  -}
  = OfferFile TransferType FileMetadata
  deriving (Eq, Show)

-- | Signal intent to resume DCC file transfer at specific position
data TryResumeFile
  {-| DCC:

      > DCC RESUME <fileName> <port> <position>

      Reverse DCC:

      > DCC RESUME <fileName> 0 <position> <token>
    -}
  = TryResumeFile TransferType FileMetadata FileOffset
  deriving (Eq, Show)

-- | Signal acceptance to resume DCC file transfer at specific position
data AcceptResumeFile
  {-| DCC:

      > DCC ACCEPT <fileName> <port> <position>

      Reverse DCC:

      > DCC ACCEPT <fileName> 0 <position> <token>
  -}
  = AcceptResumeFile TransferType FileMetadata FileOffset
  deriving (Eq, Show)

-- | Signal readiness to accept a connection (only Reverse DCC)
data OfferFileSink
  {-| Reverse DCC:

      > DCC SEND <fileName> <ip> <port> <fileSize> <token>
  -}
  = OfferFileSink Token FileMetadata IPv4 PortNumber
  deriving (Eq, Show)

-- | Type of a DCC file transfer connection
data TransferType
  -- | Connection where the owner of the file offers a socket to connect to
  = Active IPv4 PortNumber
  -- | Connection where the recipient of the file offers a socket to connect to
  | Passive IPv4 Token
  deriving (Eq, Show)

-- | Properties of a file
data FileMetadata = FileMetadata { fileName :: Path Rel File
                                 , fileSize :: Maybe FileOffset }
  deriving (Eq, Show)

-- | An identifier for knowing which negotiation a request belongs to
newtype Token = Token ByteString
  deriving (Eq, Show)

newtype FileOffset = FileOffset { toWord :: Word64 }
  deriving (Eq, Ord, Num, Integral, Enum, Real, Bounded)

instance Show FileOffset where
  show = show . toWord

-- | Class for types that can be sent as CTCP commands
class CtcpCommand a where
    encodeCtcp :: a -> CTCPByteString

instance CtcpCommand ByteString where
    encodeCtcp = encodeCTCP

instance CtcpCommand OpenChat where
    encodeCtcp = encodeCtcp . encodeOpenChat

instance CtcpCommand CloseChat where
    encodeCtcp = encodeCtcp . encodeChatClose

instance CtcpCommand OfferFile where
    encodeCtcp = encodeCtcp . encodeOfferFile

instance CtcpCommand TryResumeFile where
    encodeCtcp = encodeCtcp . encodeTryResumeFile

instance CtcpCommand AcceptResumeFile where
    encodeCtcp = encodeCtcp . encodeAcceptResume

instance CtcpCommand OfferFileSink where
    encodeCtcp = encodeCtcp . encodeOfferFileSink

runParser :: Parser a -> CTCPByteString -> Either String a
runParser p = parseOnly p . decodeCTCP

parseService :: Parser Service
parseService = Messaging <$> parseOpenChat
           <|> FileTransfer <$> parseOfferFile

encodeService :: Service -> ByteString
encodeService (Messaging m) = encodeOpenChat m
encodeService (FileTransfer o) = encodeOfferFile o

parseOpenChat :: Parser OpenChat
parseOpenChat = choice [ do "DCC CHAT chat "
                            (i, p) <- parseSocket
                            endOfInput
                            return $ Chat i p
                       , do "DCC CHAT wboard "
                            (i, p) <- parseSocket
                            endOfInput
                            return $ Whiteboard i p
                       ]

encodeOpenChat :: OpenChat -> ByteString
encodeOpenChat (Chat i p) = "DCC CHAT chat " <> encodeSocket (i, p)
encodeOpenChat (Whiteboard i p) = "DCC CHAT wboard " <> encodeSocket (i, p)

parseCloseChat :: Parser CloseChat
parseCloseChat = do "DCC CLOSE"
                    endOfInput
                    return CloseChat

encodeChatClose :: CloseChat -> ByteString
encodeChatClose _ = "DCC CLOSE"

parseOfferFile :: Parser OfferFile
parseOfferFile =
    do "DCC SEND "
       fn <- parseFileName
       space
       i <- parseIpBigEndian
       space
       choice [ do p <- parseTcpPort
                   fs <- Just <$> (space *> parseFileOffset)
                     <|> return Nothing
                   endOfInput
                   return (OfferFile
                              (Active i p)
                              (FileMetadata fn fs))
              , do "0"
                   space
                   fs <- parseFileOffset
                   space
                   t <- parseToken
                   endOfInput
                   return (OfferFile
                              (Passive i t)
                              (FileMetadata fn (Just fs)))
              ]

encodeOfferFile :: OfferFile -> ByteString
encodeOfferFile (OfferFile (Active i p) (FileMetadata fn fs)) =
    "DCC SEND "
 <> encodeFileName fn
 <> " " <> encodeIpBigEndian i
 <> " " <> encodeTcpPort p
 <> appendSpacedIfJust (encodeFileOffset <$> fs)
encodeOfferFile (OfferFile (Passive i t) (FileMetadata fn fs)) =
    "DCC SEND "
 <> encodeFileName fn
 <> " " <> encodeIpBigEndian i
 <> " 0"
 <> appendSpacedIfJust (encodeFileOffset <$> fs)
 <> " " <> encodeToken t

parseTryResumeFile :: OfferFile -> Parser TryResumeFile
parseTryResumeFile (OfferFile tt f) =
    do "DCC RESUME"
       space
       fn' <- parseFileName
       when (fn' /= fileName f) $
         fail "File name for resume didn't match file name in offer."
       space
       pos <- case tt of
                Active _ p -> do
                    string (encodeTcpPort p)
                    space
                    parseFileOffset
                Passive _ t -> do
                    "0"
                    space
                    pos <- parseFileOffset
                    space
                    string (encodeToken t)
                    return pos
       endOfInput
       return (TryResumeFile tt f pos)


encodeTryResumeFile :: TryResumeFile -> ByteString
encodeTryResumeFile (TryResumeFile (Active _ p) (FileMetadata fn _) pos) =
    "DCC RESUME "
 <> encodeFileName fn
 <> " " <> encodeTcpPort p
 <> " " <> encodeFileOffset pos
encodeTryResumeFile (TryResumeFile (Passive _ t) (FileMetadata fn _) pos) =
    "DCC RESUME "
 <> encodeFileName fn
 <> " 0 "
 <> encodeFileOffset pos
 <> " " <> encodeToken t

parseAcceptResumeFile :: TryResumeFile -> Parser AcceptResumeFile
parseAcceptResumeFile (TryResumeFile tt f _) =
    do "DCC ACCEPT "
       fn' <- parseFileName
       when (fn' /= fileName f) $
         fail "File name for accepting resume didn't match file name in offer."
       space
       ackPos <- case tt of
                   Active _ p -> do
                       string (encodeTcpPort p)
                       space
                       parseFileOffset
                   Passive _ t -> do
                       "0"
                       space
                       ackPos <- parseFileOffset
                       space
                       string (encodeToken t)
                       return ackPos
       endOfInput
       return (AcceptResumeFile tt f ackPos)

encodeAcceptResume :: AcceptResumeFile -> ByteString
encodeAcceptResume (AcceptResumeFile (Active _ p) (FileMetadata fn _) pos) =
    "DCC ACCEPT "
 <> encodeFileName fn
 <> " " <> encodeTcpPort p
 <> " " <> encodeFileOffset pos
encodeAcceptResume (AcceptResumeFile (Passive _ t) (FileMetadata fn _) pos) =
    "DCC ACCEPT "
 <> encodeFileName fn
 <> " 0 "
 <> encodeFileOffset pos
 <> " " <> encodeToken t

parseOfferFileSink :: AcceptResumeFile -> Parser (Maybe OfferFileSink)
parseOfferFileSink (AcceptResumeFile (Passive _ t) f _) =
    do "DCC SEND "
       fn <- parseFileName
       when (fn /= fileName f) $
         fail "File name for accepting resume didn't match file name in offer."
       space
       (i, p) <- parseSocket
       string (appendSpacedIfJust (encodeFileOffset <$> fileSize f))
       space
       string (encodeToken t)
       endOfInput
       return (Just (OfferFileSink t f i p))
parseOfferFileSink _ = return Nothing

encodeOfferFileSink :: OfferFileSink -> ByteString
encodeOfferFileSink (OfferFileSink t (FileMetadata fn fs) i p) =
    "DCC SEND "
 <> encodeFileName fn
 <> " " <> encodeSocket (i, p)
 <> appendSpacedIfJust (encodeFileOffset <$> fs)
 <> " " <> encodeToken t

parseSocket :: Parser (IPv4, PortNumber)
parseSocket =
    do i <- parseIpBigEndian
       space
       p <- parseTcpPort
       return (i, p)

encodeSocket :: (IPv4, PortNumber) -> ByteString
encodeSocket (i, p) = encodeIpBigEndian i <> " " <> encodeTcpPort p

parseFileName :: Parser (Path Rel File)
parseFileName = do name <- "\"" *> takeWhile1 (/= '"') <* "\""
                       <|> takeWhile1 (/= ' ')
                   case parseRelOrAbs (UTF8.toString name) of
                     Nothing -> fail "Could not parse file name."
                     Just path -> return path
  where parseRelOrAbs n = parseRelFile n
                      <|> filename <$> parseAbsFile n

encodeFileName :: Path Rel File -> ByteString
encodeFileName = pack . fromRelFile

parseIpBigEndian :: Parser IPv4
parseIpBigEndian = fromBigEndianIp <$> parseBoundedInteger 0 4294967295

encodeIpBigEndian :: IPv4 -> ByteString
encodeIpBigEndian = pack . show . toBigEndianIp

parseTcpPort :: Parser PortNumber
parseTcpPort = fromInteger <$> parseBoundedInteger 1 65535

encodeTcpPort :: PortNumber -> ByteString
encodeTcpPort = pack . show

parseFileOffset :: Parser FileOffset
parseFileOffset = FileOffset <$> decimal

encodeFileOffset :: FileOffset -> ByteString
encodeFileOffset = pack . show . toWord

parseToken :: Parser Token
parseToken = Token <$> takeByteString

encodeToken :: Token -> ByteString
encodeToken (Token t) = t

parseBoundedInteger :: Integer -> Integer -> Parser Integer
parseBoundedInteger lower upper = do
    num <- decimal
    when (num < lower || num > upper) $
         fail ("Failed to parse " ++ show num ++ ", not in range [" ++
               show lower ++ ", " ++ show upper ++ "].")
    return num

appendSpacedIfJust :: Maybe ByteString -> ByteString
appendSpacedIfJust (Just x) = " " <> x
appendSpacedIfJust Nothing = mempty

fromBigEndianIp :: Integer -> IPv4
fromBigEndianIp = fromHostAddress . byteSwap32 . fromIntegral

toBigEndianIp :: IPv4 -> Integer
toBigEndianIp = fromIntegral . byteSwap32 . toHostAddress
