{-# LANGUAGE TemplateHaskell #-}

module Network.IRC.DCCTest where

import           Network.IRC.DCC.Internal

import           Data.Attoparsec.ByteString.Char8 (IResult (Done, Fail), Parser,
                                                   endOfInput, maybeResult,
                                                   parse)
import           Data.ByteString.Char8            (pack)
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
                   pack "0" ~> parseIpBigEndian
                       `shouldParse` toIPv4 [0, 0, 0, 0]

                it "Max IPv4" $
                   pack "4294967295" ~> parseIpBigEndian
                       `shouldParse` toIPv4 [255, 255, 255, 255]

                it "Local IPv4" $
                   pack "3232235521" ~> parseIpBigEndian
                       `shouldParse` toIPv4 [192, 168, 0, 1]

                it "Public IPv4" $
                   pack "134743044" ~> parseIpBigEndian
                       `shouldParse` toIPv4 [8, 8, 4, 4]

                it "IPv4 at beginning of stream" $
                   pack "0abcd" ~?> parseIpBigEndian
                       `leavesUnconsumed` pack "abcd"

            describe "[FAILURE]" $ do

                it "Negative IPv4" $
                   parseIpBigEndian `shouldFailOn` pack "-1"

                it "Bigger than max IPv4" $
                   parseIpBigEndian `shouldFailOn` pack "4294967296"

                it "Max IPv4 with additional digit" $
                   parseIpBigEndian `shouldFailOn` pack "42949672950"

                it "Non-digits" $
                   parseIpBigEndian `shouldFailOn` pack "abcd"

                it "When not at beginning of stream" $
                   parseIpBigEndian `shouldFailOn` pack " 0"

        describe "File name" $ do

            describe "[SUCCESS]" $ do

                it "Without extension" $
                    pack "filename" ~> parseFileName
                        `shouldParse` $(mkRelFile "filename")

                it "With extension" $
                    pack "filename.txt" ~> parseFileName
                        `shouldParse` $(mkRelFile "filename.txt")

                it "Quoted with extension" $
                    pack "\"filename.txt\"" ~> parseFileName
                        `shouldParse` $(mkRelFile "filename.txt")

                it "Quoted with space" $
                    pack "\"file name.txt\"" ~> parseFileName
                        `shouldParse` $(mkRelFile "file name.txt")

                it "UTF8 with em space" $
                    UTF8.fromString "file\8195name.txt" ~> parseFileName
                        `shouldParse` $(mkRelFile "file\8195name.txt")

                it "UTF8 with skin tone modifier" $
                    UTF8.fromString "\128110\127997" ~> parseFileName
                        `shouldParse` $(mkRelFile "\128110\127997")

                it "Quoted UTF8 with space" $
                    UTF8.fromString "\"file\8195 name.txt\"" ~> parseFileName
                        `shouldParse` $(mkRelFile "file\8195 name.txt")

                it "From absolute unix path" $
                    pack "/home/user/filename.txt" ~> parseFileName
                        `shouldParse` $(mkRelFile "filename.txt")

                it "From quoted absolute unix path" $
                    pack "\"/home/user/filename.txt\"" ~> parseFileName
                        `shouldParse` $(mkRelFile "filename.txt")

                it "At beginning of stream" $
                    pack "filename 122350" ~?> parseFileName
                        `leavesUnconsumed` pack " 122350"

-- TODO Not sure exactly what to do with this yet. On Unix the whole thing
--      could be a file name, while on windows it is obviously an absolute path
--                 it "File name from absoulte unix path" $
--                     ("c:\\Users\\user\\filename.txt" ~> parseFileName
--                         `shouldParse` $(mkRelFile "filename.txt")

            describe "[FAILURE]" $ do

                it "ASCII filename with space" $
                    pack "file name.txt" ~?> parseFileName
                        `leavesUnconsumed` pack " name.txt"

                it "Not at beginning of stream" $
                    parseFileName `shouldFailOn` pack " filename"
