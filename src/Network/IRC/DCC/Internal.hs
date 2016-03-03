{-# LANGUAGE OverloadedStrings #-}

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
  -- | Text messages exchange
  = Chat IPv4 PortNumber
  -- | Drawing commands exchange
  | Whiteboard IPv4 PortNumber
  deriving (Eq, Show)

-- | Signal intent to close DCC chat connection
data CloseChat = CloseChat

-- | DCC file transfer instructions
data OfferFile = OfferFile TransferType FileMetadata
  deriving (Eq, Show)

-- | Signal intent to resume DCC file transfer at specific position
data TryResumeFile = TryResumeFile TransferType FileMetadata FileOffset
  deriving (Eq, Show)

-- | Signal acceptance to resume DCC file transfer at specific position
data AcceptResumeFile = AcceptResumeFile TransferType FileMetadata FileOffset
  deriving (Eq, Show)

-- | Signal readiness to accept a connection
data OfferFileSink = OfferFileSink Token FileMetadata IPv4 PortNumber
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
data Token = Token ByteString
  deriving (Eq, Show)

type FileOffset = Word64

class CtcpCommand a where
    encodeCtcp :: a -> CTCPByteString

instance CtcpCommand ByteString where
    encodeCtcp = encodeCTCP

instance CtcpCommand OpenChat where
    encodeCtcp = encodeCtcp . encodeChatOffer

instance CtcpCommand CloseChat where
    encodeCtcp = encodeCtcp . encodeChatClose

instance CtcpCommand OfferFile where
    encodeCtcp = encodeCtcp . encodeOffer

instance CtcpCommand TryResumeFile where
    encodeCtcp = encodeCtcp . encodeTryResume

instance CtcpCommand AcceptResumeFile where
    encodeCtcp = encodeCtcp . encodeAcceptResume

instance CtcpCommand OfferFileSink where
    encodeCtcp = encodeCtcp . encodeOfferSink

runParser :: Parser a -> CTCPByteString -> Either String a
runParser p = parseOnly p . decodeCTCP

decodeService :: Parser Service
decodeService = choice [ do m <- decodeChatOffer
                            return $ Messaging m
                       , do o <- decodeFileOffer
                            return $ FileTransfer o
                       ]

encodeService :: Service -> ByteString
encodeService (Messaging m) = encodeChatOffer m
encodeService (FileTransfer o) = encodeOffer o

decodeChatOffer :: Parser OpenChat
decodeChatOffer = choice [ do "DCC CHAT chat "
                              (i, p) <- decodeSocket
                              endOfInput
                              return $ Chat i p
                         , do "DCC CHAT wboard "
                              (i, p) <- decodeSocket
                              endOfInput
                              return $ Whiteboard i p
                         ]

encodeChatOffer :: OpenChat -> ByteString
encodeChatOffer (Chat i p) = "DCC CHAT chat " <> encodeSocket i p
encodeChatOffer (Whiteboard i p) = "DCC CHAT wboard " <> encodeSocket i p

decodeChatClose :: Parser CloseChat
decodeChatClose = do "DCC CLOSE"
                     endOfInput
                     return CloseChat

encodeChatClose :: CloseChat -> ByteString
encodeChatClose _ = "DCC CLOSE"

decodeFileOffer :: Parser OfferFile
decodeFileOffer = do "DCC SEND "
                     fn <- decodeFileName
                     space
                     i <- decodeIpBigEndian
                     space
                     choice [ do p <- decodeTcpPort
                                 fs <- choice [ do space
                                                   fs <- decodeFileOffset
                                                   return $ Just fs
                                              , return Nothing
                                              ]
                                 endOfInput
                                 return (OfferFile
                                            (Active i p)
                                            (FileMetadata fn fs))
                            , do "0"
                                 space
                                 fs <- decodeFileOffset
                                 space
                                 t <- decodeToken
                                 endOfInput
                                 return (OfferFile
                                            (Passive i t)
                                            (FileMetadata fn (Just fs)))
                            ]

encodeOffer :: OfferFile -> ByteString
encodeOffer (OfferFile (Active i p) (FileMetadata fn fs)) =
    "DCC SEND "
 <> encodeFileName fn
 <> " " <> encodeIpBigEndian i
 <> " " <> encodeTcpPort p
 <> appendSpacedIfJust (encodeFileOffset <$> fs)
encodeOffer (OfferFile (Passive i t) (FileMetadata fn fs)) =
    "DCC SEND "
 <> encodeFileName fn
 <> " " <> encodeIpBigEndian i
 <> " 0"
 <> appendSpacedIfJust (encodeFileOffset <$> fs)
 <> " " <> encodeToken t

decodeTryResume :: OfferFile -> Parser TryResumeFile
decodeTryResume (OfferFile tt f) =
    do "DCC RESUME"
       space
       fn' <- decodeFileName
       when (fn' /= fileName f) $
         fail "File name for resume didn't match file name in offer."
       space
       pos <- case tt of
                Active _ p -> do
                    string (encodeTcpPort p)
                    space
                    decodeFileOffset
                Passive _ t -> do
                    "0"
                    space
                    pos <- decodeFileOffset
                    space
                    string (encodeToken t)
                    return pos
       endOfInput
       return (TryResumeFile tt f pos)


encodeTryResume :: TryResumeFile -> ByteString
encodeTryResume (TryResumeFile (Active _ p) (FileMetadata fn _) pos) =
    "DCC RESUME "
 <> encodeFileName fn
 <> " " <> encodeTcpPort p
 <> " " <> encodeFileOffset pos
encodeTryResume (TryResumeFile (Passive _ t) (FileMetadata fn _) pos) =
    "DCC RESUME "
 <> encodeFileName fn
 <> " 0 "
 <> encodeFileOffset pos
 <> " " <> encodeToken t

decodeAcceptResume :: TryResumeFile -> Parser AcceptResumeFile
decodeAcceptResume (TryResumeFile tt f _) =
    do "DCC ACCEPT "
       fn' <- decodeFileName
       when (fn' /= fileName f) $
         fail "File name for accepting resume didn't match file name in offer."
       space
       ackPos <- case tt of
                   Active _ p -> do
                       string (encodeTcpPort p)
                       space
                       decodeFileOffset
                   Passive _ t -> do
                       "0"
                       space
                       ackPos <- decodeFileOffset
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

decodeOfferSink :: AcceptResumeFile -> Parser (Maybe OfferFileSink)
decodeOfferSink (AcceptResumeFile (Passive _ t) f _) =
    do "DCC SEND "
       fn <- decodeFileName
       when (fn /= fileName f) $
         fail "File name for accepting resume didn't match file name in offer."
       space
       (i, p) <- decodeSocket
       string (appendSpacedIfJust (encodeFileOffset <$> fileSize f))
       space
       string (encodeToken t)
       endOfInput
       return (Just (OfferFileSink t f i p))
decodeOfferSink _ = return Nothing

encodeOfferSink :: OfferFileSink -> ByteString
encodeOfferSink (OfferFileSink t (FileMetadata fn fs) i p) =
    "DCC SEND "
 <> encodeFileName fn
 <> " " <> encodeSocket i p
 <> appendSpacedIfJust (encodeFileOffset <$> fs)
 <> " " <> encodeToken t

decodeSocket :: Parser (IPv4, PortNumber)
decodeSocket =
    do i <- decodeIpBigEndian
       space
       p <- decodeTcpPort
       return (i, p)

encodeSocket :: IPv4 -> PortNumber -> ByteString
encodeSocket i p = encodeIpBigEndian i <> " " <> encodeTcpPort p

decodeFileName :: Parser (Path Rel File)
decodeFileName = do name <- "\"" *> takeWhile1 (/= '"') <* "\""
                        <|> takeWhile1 (/= ' ')
                    case parseRelOrAbs (UTF8.toString name) of
                      Nothing -> fail "Could not parse file name."
                      Just path -> return path
  where parseRelOrAbs n = parseRelFile n
                      <|> filename <$> parseAbsFile n

encodeFileName :: Path Rel File -> ByteString
encodeFileName = pack . fromRelFile

decodeIpBigEndian :: Parser IPv4
decodeIpBigEndian = fromBigEndianIp <$> parseBoundedInteger 0 4294967295

encodeIpBigEndian :: IPv4 -> ByteString
encodeIpBigEndian = pack . show . toBigEndianIp

decodeTcpPort :: Parser PortNumber
decodeTcpPort = fromInteger <$> parseBoundedInteger 1 65535

encodeTcpPort :: PortNumber -> ByteString
encodeTcpPort = pack . show

decodeFileOffset :: Parser FileOffset
decodeFileOffset = decimal

encodeFileOffset :: FileOffset -> ByteString
encodeFileOffset = pack . show

decodeToken :: Parser Token
decodeToken = Token <$> takeByteString

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
