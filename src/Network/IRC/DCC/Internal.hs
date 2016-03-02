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
  = Messaging ChatProtocol
  -- | Offer file transfer
  | FileTransfer Offer
    deriving (Eq, Show)

-- | Type of DCC chat service
data ChatProtocol
  -- | Plain text messages exchange
  = Chat IPv4 PortNumber
  -- | Drawing commands exchange
  | Whiteboard IPv4 PortNumber
    deriving (Eq, Show)

-- | Signal intent to close DCC chat connection
data CloseChat = CloseChat

-- | DCC file transfer instructions
data Offer = Offer TransferType FileMetadata
             deriving (Eq, Show)

-- | Signal intent to resume DCC file transfer at specific position
data TryResume = TryResume TransferType FileMetadata FileOffset
                 deriving (Eq, Show)

-- | Signal acceptance to resume DCC file transfer at specific position
data AcceptResume = AcceptResume TransferType FileMetadata FileOffset
                    deriving (Eq, Show)

-- | Signal readiness to accept a connection
data OfferSink = OfferSink TransferType FileMetadata IPv4 PortNumber
                 deriving (Eq, Show)

-- | Type of a DCC file transfer connection
data TransferType
  -- | Connection where the owner of the file offers a socket to connect to
  = Active IPv4 PortNumber
  -- | Connection where the recipient of the file offers a socket to connect to
  | Passive IPv4 Token
    deriving (Eq, Show)

-- | Properties of the file to be transfered
data FileMetadata = FileMetadata { fileName :: Path Rel File
                                 , fileSize :: Maybe FileOffset }
                    deriving (Eq, Show)

-- | An identifier for knowing which negotiation a request belongs to
data Token = Token ByteString
             deriving (Eq, Show)

type FileOffset = Word64

runParser :: Parser a -> CTCPByteString -> Either String a
runParser p = parseOnly p . decodeCTCP

decodeService :: Parser Service
decodeService = do "DCC "
                   choice [ do "CHAT "
                               m <- decodeChatOffer
                               return $ Messaging m
                          , do "SEND "
                               o <- decodeFileOffer
                               return $ FileTransfer o
                          ]

encodeService :: Service -> CTCPByteString
encodeService (Messaging m) = encodeCTCP $
    "DCC CHAT "
 <> encodeChatOffer m
encodeService (FileTransfer o) = encodeCTCP $
    "DCC SEND "
 <> encodeFileOffer o

decodeChatClose :: Parser CloseChat
decodeChatClose = do "DCC CLOSE"
                     endOfInput
                     return CloseChat

encodeChatClose :: CloseChat -> CTCPByteString
encodeChatClose _ = encodeCTCP "DCC CLOSE"

decodeChatOffer :: Parser ChatProtocol
decodeChatOffer = choice [ do "chat "
                              (i, p) <- decodeSocket
                              endOfInput
                              return $ Chat i p
                         , do "wboard "
                              (i, p) <- decodeSocket
                              endOfInput
                              return $ Whiteboard i p
                         ]

encodeChatOffer :: ChatProtocol -> ByteString
encodeChatOffer (Chat i p) = "chat " <> encodeSocket i p
encodeChatOffer (Whiteboard i p) = "wboard " <> encodeSocket i p

decodeFileOffer :: Parser Offer
decodeFileOffer = do fn <- decodeFileName
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
                                 return (Offer (Active i p)
                                               (FileMetadata fn fs))
                            , do "0"
                                 space
                                 fs <- decodeFileOffset
                                 space
                                 t <- decodeToken
                                 endOfInput
                                 return (Offer (Passive i t)
                                               (FileMetadata fn (Just fs)))
                            ]

encodeFileOffer :: Offer -> ByteString
encodeFileOffer (Offer (Active i p) (FileMetadata fn fs)) =
    encodeFileName fn
 <> " " <> encodeIpBigEndian i
 <> " " <> encodeTcpPort p
 <> appendSpacedIfJust (encodeFileOffset <$> fs)
encodeFileOffer (Offer (Passive i t) (FileMetadata fn fs)) =
    encodeFileName fn
 <> " " <> encodeIpBigEndian i
 <> " 0"
 <> appendSpacedIfJust (encodeFileOffset <$> fs)
 <> " " <> encodeToken t

decodeTryResume :: Offer -> Parser TryResume
decodeTryResume (Offer tt f) =
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
       return (TryResume tt f pos)


encodeTryResume :: TryResume -> CTCPByteString
encodeTryResume (TryResume (Active _ p) (FileMetadata fn _) pos) =
    encodeCTCP $ "DCC RESUME "
              <> encodeFileName fn
              <> " " <> encodeTcpPort p
              <> " " <> encodeFileOffset pos
encodeTryResume (TryResume (Passive _ t) (FileMetadata fn _) pos) =
    encodeCTCP $ "DCC RESUME "
              <> encodeFileName fn
              <> " 0 "
              <> encodeFileOffset pos
              <> " " <> encodeToken t

decodeAcceptResume :: TryResume -> Parser AcceptResume
decodeAcceptResume (TryResume tt f _) =
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
       return (AcceptResume tt f ackPos)

encodeAcceptResume :: AcceptResume -> CTCPByteString
encodeAcceptResume (AcceptResume (Active _ p) (FileMetadata fn _) pos) =
    encodeCTCP $ "DCC ACCEPT "
              <> encodeFileName fn
              <> " " <> encodeTcpPort p
              <> " " <> encodeFileOffset pos
encodeAcceptResume (AcceptResume (Passive _ t) (FileMetadata fn _) pos) =
    encodeCTCP $ "DCC ACCEPT "
              <> encodeFileName fn
              <> " 0 "
              <> encodeFileOffset pos
              <> " " <> encodeToken t

decodeOfferSink :: AcceptResume -> Parser (Maybe OfferSink)
decodeOfferSink (AcceptResume tt@(Passive _ t) f _) =
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
       return (Just (OfferSink tt f i p))
decodeOfferSink _ = return Nothing

encodeOfferSink :: OfferSink -> Maybe CTCPByteString
encodeOfferSink (OfferSink (Passive _ t) (FileMetadata fn fs) i p) = Just $
    encodeCTCP $ "DCC SEND "
              <> encodeFileName fn
              <> " " <> encodeSocket i p
              <> appendSpacedIfJust (encodeFileOffset <$> fs)
              <> " " <> encodeToken t
encodeOfferSink _ = Nothing

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

appendSpacedIfJust :: Maybe ByteString -> ByteString
appendSpacedIfJust (Just x) = " " <> x
appendSpacedIfJust Nothing = ""

parseBoundedInteger :: Integer -> Integer -> Parser Integer
parseBoundedInteger lower upper = do
    num <- decimal
    when (num < lower || num > upper) $
         fail ("Failed to parse " ++ show num ++ ", not in range [" ++
               show lower ++ ", " ++ show upper ++ "].")
    return num

fromBigEndianIp :: Integer -> IPv4
fromBigEndianIp = fromHostAddress . byteSwap32 . fromIntegral

toBigEndianIp :: IPv4 -> Integer
toBigEndianIp = fromIntegral . byteSwap32 . toHostAddress
