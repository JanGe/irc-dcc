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
import           Data.List                        (intersperse)
import           Data.Word                        (Word64)
import           Network.IRC.CTCP                 (CTCPByteString, decodeCTCP,
                                                   encodeCTCP)
import           Network.Socket                   (PortNumber)
import           Path                             (File, Path, Rel, filename,
                                                   fromRelFile, parseAbsFile,
                                                   parseRelFile)

data Service = Messaging
             | FileTransfer
               deriving (Eq, Show)

data ChatProtocol = Chat IPv4 PortNumber
                  | Whiteboard IPv4 PortNumber
                    deriving (Eq, Show)

data Offer = Offer TransferType FileMetadata
             deriving (Eq, Show)

data TryResume = TryResume TransferType FileMetadata FileOffset
                 deriving (Eq, Show)

data AcceptResume = AcceptResume TransferType FileMetadata FileOffset
                    deriving (Eq, Show)

data OfferSink = OfferSink TransferType FileMetadata IPv4 PortNumber
                 deriving (Eq, Show)

data TransferType = Active IPv4 PortNumber
                  | Passive IPv4 Token
                    deriving (Eq, Show)

data FileMetadata = FileMetadata (Path Rel File) FileOffset
                    deriving (Eq, Show)

data Token = Token ByteString
             deriving (Eq, Show)

type FileOffset = Word64

runParser :: Parser a -> CTCPByteString -> Either String a
runParser p = parseOnly p . decodeCTCP

decodeOffer :: Parser Offer
decodeOffer = do "DCC SEND"
                 space
                 fn <- decodeFileName
                 space
                 i <- decodeIpBigEndian
                 space
                 choice [ do p <- decodeTcpPort
                             space
                             fs <- decodeFileOffset
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
                                           (FileMetadata fn fs))
                        ]

encodeOffer :: Offer -> CTCPByteString
encodeOffer (Offer (Active i p) (FileMetadata fn fs)) = encodeCTCP $
   concatSpace [ "DCC SEND"
               , encodeFileName fn
               , encodeIpBigEndian i
               , encodeTcpPort p
               , encodeFileOffset fs ]
encodeOffer (Offer (Passive i t) (FileMetadata fn fs)) = encodeCTCP $
   concatSpace [ "DCC SEND"
               , encodeFileName fn
               , encodeIpBigEndian i
               , "0"
               , encodeFileOffset fs
               , encodeToken t ]

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
    encodeCTCP $ concatSpace [ "DCC RESUME"
                             , encodeFileName fn
                             , encodeTcpPort p
                             , encodeFileOffset pos ]
encodeTryResume (TryResume (Passive _ t) (FileMetadata fn _) pos) =
    encodeCTCP $ concatSpace [ "DCC RESUME"
                             , encodeFileName fn
                             , "0"
                             , encodeFileOffset pos
                             , encodeToken t ]

decodeAcceptResume :: TryResume -> Parser AcceptResume
decodeAcceptResume (TryResume tt f _) =
    do "DCC ACCEPT"
       space
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
    encodeCTCP $ concatSpace [ "DCC ACCEPT"
                             , encodeFileName fn
                             , encodeTcpPort p
                             , encodeFileOffset pos ]
encodeAcceptResume (AcceptResume (Passive _ t) (FileMetadata fn _) pos) =
    encodeCTCP $ concatSpace [ "DCC ACCEPT"
                             , encodeFileName fn
                             , "0"
                             , encodeFileOffset pos
                             , encodeToken t ]

decodeOfferSink :: AcceptResume -> Parser (Maybe OfferSink)
decodeOfferSink (AcceptResume tt@(Passive _ t) f _) =
    do "DCC SEND"
       space
       fn <- decodeFileName
       when (fn /= fileName f) $
         fail "File name for accepting resume didn't match file name in offer."
       space
       i <- decodeIpBigEndian
       space
       p <- decodeTcpPort
       space
       string (encodeFileOffset (fileSize f))
       space
       string (encodeToken t)
       endOfInput
       return (Just (OfferSink tt f i p))
decodeOfferSink _ = return Nothing

encodeOfferSink :: OfferSink -> Maybe CTCPByteString
encodeOfferSink (OfferSink (Passive _ t) (FileMetadata fn fs) i p) = Just $
    encodeCTCP $ concatSpace [ "DCC SEND"
                             , encodeFileName fn
                             , encodeIpBigEndian i
                             , encodeTcpPort p
                             , encodeFileOffset fs
                             , encodeToken t ]
encodeOfferSink _ = Nothing

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

concatSpace :: [ByteString] -> ByteString
concatSpace = mconcat . intersperse " "

fromBigEndianIp :: Integer -> IPv4
fromBigEndianIp = fromHostAddress . byteSwap32 . fromIntegral

toBigEndianIp :: IPv4 -> Integer
toBigEndianIp = fromIntegral . byteSwap32 . toHostAddress

fileMetadata :: Offer -> FileMetadata
fileMetadata (Offer _ f) = f

fileName :: FileMetadata -> Path Rel File
fileName (FileMetadata fn _) = fn

fileSize :: FileMetadata -> FileOffset
fileSize (FileMetadata _ fs) = fs
