{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Network.IRC.DCCTest where

import           Network.IRC.DCC

import           Data.Attoparsec.ByteString.Char8 (IResult (Done, Fail), Parser,
                                                   endOfInput, maybeResult,
                                                   parse)
import           Data.ByteString.Char8            (ByteString)
import qualified Data.ByteString.UTF8             as UTF8 (fromString)
import           Data.IP                          (IPv4, toIPv4)
import           Path                             (mkRelFile)
import           Test.Hspec.Attoparsec
import           Test.Tasty                       (testGroup)
import           Test.Tasty.Hspec                 (SpecWith, describe, it,
                                                   testSpec)

spec = testSpec "DCC message serialization" $

    describe "Parsing single elements" $ do

        describe "Network byte order IP" $ do

            describe "[SUCCESS]" $ do

                it "Min IPv4" $
                   ("0" :: ByteString) ~> parseIpBE
                       `shouldParse` toIPv4 [0, 0, 0, 0]

                it "Max IPv4" $
                   ("4294967295" :: ByteString) ~> parseIpBE
                       `shouldParse` toIPv4 [255, 255, 255, 255]

                it "Local IPv4" $
                   ("3232235521" :: ByteString) ~> parseIpBE
                       `shouldParse` toIPv4 [192, 168, 0, 1]

                it "Public IPv4" $
                   ("134743044" :: ByteString) ~> parseIpBE
                       `shouldParse` toIPv4 [8, 8, 4, 4]

                it "IPv4 at beginning of stream" $
                   ("0abcd" :: ByteString) ~?> parseIpBE
                       `leavesUnconsumed` "abcd"

            describe "[FAILURE]" $ do

                it "Negative IPv4" $
                   parseIpBE `shouldFailOn` ("-1" :: ByteString)

                it "Bigger than max IPv4" $
                   parseIpBE `shouldFailOn` ("4294967296" :: ByteString)

                it "Max IPv4 with additional digit" $
                   parseIpBE `shouldFailOn` ("42949672950" :: ByteString)

                it "Non-digits" $
                   parseIpBE `shouldFailOn` ("abcd" :: ByteString)

                it "When not at beginning of stream" $
                   parseIpBE `shouldFailOn` (" 0" :: ByteString)

        describe "File name" $ do

            describe "[SUCCESS]" $ do

                it "File name without extension" $
                    ("filename" :: ByteString) ~> parseFileName
                        `shouldParse` $(mkRelFile "filename")

                it "File name with extension" $
                    ("filename.txt" :: ByteString) ~> parseFileName
                        `shouldParse` $(mkRelFile "filename.txt")

                it "Quoted file name with extension" $
                    ("\"filename.txt\"" :: ByteString) ~> parseFileName
                        `shouldParse` $(mkRelFile "filename.txt")

                it "Quoted file name with space" $
                    ("\"file name.txt\"" :: ByteString) ~> parseFileName
                        `shouldParse` $(mkRelFile "file name.txt")

                it "UTF8 file name with em space" $
                    UTF8.fromString "file\8195name.txt" ~> parseFileName
                        `shouldParse` $(mkRelFile "file\8195name.txt")

                it "UTF8 file name with skin tone modifier" $
                    UTF8.fromString "\128110\127997" ~> parseFileName
                        `shouldParse` $(mkRelFile "\128110\127997")

                it "Quoted UTF8 file name with space" $
                    UTF8.fromString "\"file\8195 name.txt\"" ~> parseFileName
                        `shouldParse` $(mkRelFile "file\8195 name.txt")

                -- Currently only works when built on UNIX, see
                -- https://github.com/haskell/filepath/issues/13
                it "File name from absoulte unix path" $
                    ("/home/user/filename.txt" :: ByteString) ~> parseFileName
                        `shouldParse` $(mkRelFile "filename.txt")

-- TODO Enable when https://github.com/haskell/filepath/issues/13
--      was fixed
--                 it "File name from absoulte unix path" $
--                     ("c:\\Users\\user\\filename.txt" :: ByteString) ~> parseFileName
--                         `shouldParse` $(mkRelFile "filename.txt")

            describe "[FAILURE]" $ do

                it "ASCII filename with space" $
                    ("file name.txt" :: ByteString) ~?> parseFileName
                        `leavesUnconsumed` " name.txt"

--         describe "Parse file name" $ do
--             success parseFileName
--                 [ ("filename.txt", $(mkRelFile "filename.txt"))
--                 , ("\"filename.txt\"", $(mkRelFile "filename.txt"))
--                 , (UTF8.fromString "\128110\127997", $(mkRelFile "\128110\127997"))
--                 , ("<a href>", $(mkRelFile "<a"))
--                 , ("c:\\", $(mkRelFile "c:\\"))
--                 , ("!@#$%^&*()`~", $(mkRelFile "!@#$%^&*()`~"))
--                 , (UTF8.fromString "田中さんにあげて下さい", $(mkRelFile "田中さんにあげて下さい"))
--                 , ("\000\000\000", $(mkRelFile "\000\000\000"))
--                 , ("\r\nbla.txt", $(mkRelFile "\r\nbla.txt"))
--                 ]
--             failure parseFileName
--                     [ "/"
--                     , "bla\ETXbla"
--                     ]
