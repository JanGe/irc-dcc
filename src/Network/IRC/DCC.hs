{-# LANGUAGE OverloadedStrings #-}

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
                       , parseOffer
                       , encodeOffer
                       , parseTryResume
                       , encodeTryResume
                       , parseAcceptResume
                       , encodeAcceptResume
                       , parseOfferSink
                       , encodeOfferSink
                       , fileMetadata
                       , parseIpBE
                       , parseFileName
                       ) where

import           Control.Applicative
import           Control.Monad                    (when)
import           Data.Attoparsec.ByteString.Char8
import           Data.Binary                      (byteSwap32)
import           Data.ByteString.Char8            (ByteString, pack, unpack)
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

data TryResume = TryResume Offer FileOffset
                 deriving (Eq, Show)

data AcceptResume = AcceptResume Offer FileOffset
                    deriving (Eq, Show)

data OfferSink = OfferSink Offer IPv4 PortNumber
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

parseOffer :: Parser Offer
parseOffer = do "DCC SEND"
                space
                file <- parseFileName
                space
                ip <- parseIpBE
                space
                choice [ do port <- parseTcpPort
                            space
                            size <- parseFileOffset
                            endOfInput
                            return (Offer (Active ip port)
                                          (FileMetadata file size))
                       , do "0"
                            space
                            size <- parseFileOffset
                            space
                            token <- parseToken
                            endOfInput
                            return (Offer (Passive ip token)
                                          (FileMetadata file size))
                       ]

parseTryResume :: Offer -> Parser TryResume
parseTryResume o@(Offer (Active i p) (FileMetadata fn fs)) =
    do "DCC RESUME"
       space
       file <- parseFileName
       when (file /= fn) $
         fail "File name for resume didn't match file name in offer."
       space
       (string . pack . show) p
       space
       position <- parseFileOffset
       endOfInput
       return (TryResume o position)
parseTryResume o@(Offer (Passive i (Token t)) (FileMetadata fn fs)) =
    do "DCC RESUME"
       space
       file <- parseFileName
       when (file /= fn) $
         fail "File name for resume didn't match file name in offer."
       space
       "0"
       space
       position <- parseFileOffset
       space
       string t
       endOfInput
       return (TryResume o position)

parseAcceptResume :: TryResume -> Parser AcceptResume
parseAcceptResume (TryResume o@(Offer (Active i p) (FileMetadata fn fs)) fo) =
    AcceptResume o <$> ( "DCC ACCEPT" *> space
                      *> (string . pack . fromRelFile) fn *> space
                      *> (string . pack . show) p  *> space
                      *> parseFileOffset )
parseAcceptResume (TryResume o@(Offer (Passive i (Token t))
                                      (FileMetadata fn fs)) fo) =
    AcceptResume o <$> ( "DCC ACCEPT" *> space
                      *> (string . pack . fromRelFile) fn *> space
                      *> "0"  *> space
                      *> parseFileOffset <* space <* string t )

parseOfferSink = undefined

parseFileName :: Parser (Path Rel File)
parseFileName = do name <- "\"" *> takeWhile1 (/= '"') <* "\""
                       <|> takeWhile1 (/= ' ')
                   case parseRelOrAbs (UTF8.toString name) of
                     Nothing -> fail "Could not parse file name."
                     Just path -> return path
  where parseRelOrAbs n = parseRelFile n
                      <|> filename <$> parseAbsFile n

parseIpBE :: Parser IPv4
parseIpBE = ipFromNetworkByteOrder <$> parseBoundedInteger 0 4294967295

parseTcpPort :: Parser PortNumber
parseTcpPort = fromInteger <$> parseBoundedInteger 1 65535

parseFileOffset :: Parser FileOffset
parseFileOffset = decimal

parseBoundedInteger :: Integer -> Integer -> Parser Integer
parseBoundedInteger lower upper = do
    num <- decimal
    when (num < lower || num > upper) $
         fail ("Failed to parse " ++ show num ++ ", not in range [" ++
               show lower ++ ", " ++ show upper ++ "].")
    return num

parseToken :: Parser Token
parseToken = Token <$> takeByteString

encodeOffer :: Offer -> CTCPByteString
encodeOffer (Offer (Active i p) (FileMetadata fn fs)) = encodeCTCP $
   concatSpace [ "DCC SEND"
               , (pack . fromRelFile) fn
               , (pack . show . ipToNetworkByteOrder) i
               , (pack . show) p
               , (pack . show) fs ]
encodeOffer (Offer (Passive i (Token t)) (FileMetadata fn fs)) = encodeCTCP $
               concatSpace [ "DCC SEND"
                           , (pack . fromRelFile) fn
                           , (pack . show . ipToNetworkByteOrder) i
                           , "0"
                           , (pack . show) fs
                           , t ]

encodeTryResume :: TryResume -> CTCPByteString
encodeTryResume (TryResume (Offer (Active i p) (FileMetadata fn fs)) fo) =
    encodeCTCP $ concatSpace [ "DCC RESUME"
                             , (pack . fromRelFile) fn
                             , (pack . show) p
                             , (pack . show) fo ]
encodeTryResume (TryResume (Offer (Passive i (Token t))
                                  (FileMetadata fn fs)) fo) =
    encodeCTCP $ concatSpace [ "DCC RESUME"
                             , (pack . fromRelFile) fn
                             , "0"
                             , (pack . show) fo
                             , t ]

encodeAcceptResume :: AcceptResume -> CTCPByteString
encodeAcceptResume (AcceptResume (Offer (Active i p) (FileMetadata fn fs)) fo) =
    encodeCTCP $ concatSpace [ "DCC ACCEPT"
                             , (pack . fromRelFile) fn
                             , (pack . show) p
                             , (pack . show) fo ]
encodeAcceptResume (AcceptResume (Offer (Passive i (Token t))
                                        (FileMetadata fn fs)) fo) =
    encodeCTCP $ concatSpace [ "DCC ACCEPT"
                             , (pack . fromRelFile) fn
                             , "0"
                             , (pack . show) fo
                             , t ]

encodeOfferSink :: OfferSink -> Maybe CTCPByteString
encodeOfferSink (OfferSink (Offer (Passive _ (Token t))
                                  (FileMetadata fn fs)) i p) = Just $
    encodeCTCP $ concatSpace [ "DCC SEND"
                             , (pack . fromRelFile) fn
                             , (pack . show . ipToNetworkByteOrder) i
                             , (pack . show) p
                             , (pack . show) fs
                             , t ]
encodeOfferSink _ = Nothing

concatSpace :: [ByteString] -> ByteString
concatSpace = mconcat . intersperse " "

ipFromNetworkByteOrder :: Integer -> IPv4
ipFromNetworkByteOrder = fromHostAddress . byteSwap32 . fromIntegral

ipToNetworkByteOrder :: IPv4 -> Integer
ipToNetworkByteOrder = fromIntegral . byteSwap32 . toHostAddress

fileMetadata :: Offer -> FileMetadata
fileMetadata (Offer _ f) = f
