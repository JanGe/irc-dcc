{-# LANGUAGE TemplateHaskell #-}

module Network.IRC.DCCTest where

import           Network.IRC.DCC.Internal

import           Control.Monad            (replicateM)
import           Data.ByteString.Char8    (pack)
import           Data.IP                  (IPv4, toIPv4)
import           Network.Socket           (PortNumber)
import           Path                     (mkAbsFile, mkRelFile)
import           Test.Hspec.Attoparsec
import           Test.Tasty               (TestTree)
import           Test.Tasty.Hspec         (describe, it, testSpec)
import           Test.Tasty.QuickCheck    (Arbitrary (..), Gen, choose,
                                           elements, forAll, oneof, property)

instance Arbitrary Token where
  arbitrary = Token . pack <$> arbitrary

instance Arbitrary PortNumber where
  arbitrary = fromIntegral <$> (choose (1, 65535) :: Gen Int)

instance Arbitrary IPv4 where
  arbitrary = toIPv4 <$> replicateM 4 (choose (0, 255))

instance Arbitrary Path where
  arbitrary = elements [ Rel Simple $(mkRelFile "filename")
                       , Rel Simple $(mkRelFile "filename.txt")
                       , Rel Simple $(mkRelFile "file\8195name.txt")
                       , Rel Simple $(mkRelFile "\128110\127997")
                       , Abs Simple $(mkAbsFile "/home/user/dirname/filename.txt")
                       , Rel Quoted $(mkRelFile "filename.txt")
                       , Rel Quoted $(mkRelFile "file name.txt")
                       , Rel Quoted $(mkRelFile "file\8195 name.txt")
                       , Rel Quoted $(mkRelFile "\128110\127997")
                       , Abs Quoted $(mkAbsFile "/home/user/dir name/filename.txt")
                       ]

instance Arbitrary FileOffset where
  arbitrary = FileOffset <$> arbitrary

instance Arbitrary DccChat where
  arbitrary = oneof [ Chat <$> arbitrary <*> arbitrary
                    , ChatWhiteboard <$> arbitrary <*> arbitrary
                    ]

instance Arbitrary DccSend where
  arbitrary = oneof [ Send <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                    , SendReverseServer <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                    ]

instance Arbitrary DccResume where
  arbitrary = oneof [ Resume <$> arbitrary <*> arbitrary <*> arbitrary
                    , ResumeReverse <$> arbitrary <*> arbitrary <*> arbitrary
                    ]

instance Arbitrary DccAccept where
  arbitrary = oneof [ Accept <$> arbitrary <*> arbitrary <*> arbitrary
                    , AcceptReverse <$> arbitrary <*> arbitrary <*> arbitrary
                    ]

instance Arbitrary DccSendReverseClient where
  arbitrary = SendReverseClient
      <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

spec :: IO TestTree
spec = testSpec "DCC message serialization" $ do

  describe "Parsing single elements" $ do

    describe "Network byte order IP" $ do
      describe "[SUCCESS]" $ do
        it "parse . encode == id" $ property $ \ip ->
          ipToBigEndianBS ip ~> ipBigEndian `parseSatisfies` (== ip)
        it "At beginning of stream" $
          pack "0abcd" ~?> ipBigEndian
              `leavesUnconsumed` pack "abcd"
      describe "[FAILURE]" $ do
        it "Parse < 0.0.0.0" $
          forAll (choose (minBound, -1 :: Int)) $ \ip ->
             ipBigEndian `shouldFailOn` pack (show ip)
        it "Parse > 255.255.255.255" $
          forAll (choose (4294967296, maxBound :: Int)) $ \ip ->
             ipBigEndian `shouldFailOn` pack (show ip)
        it "Non-digits" $
          ipBigEndian `shouldFailOn` pack "abcd"
        it "When not at beginning of stream" $
          ipBigEndian `shouldFailOn` pack " 0"

    describe "File name" $ do
      describe "[SUCCESS]" $ do
        it "parse . encode == id" $ property $ \name ->
          pathToBS name ~> path `parseSatisfies` (== name)
        it "At beginning of stream" $
          pack "filename 122350" ~?> path
            `leavesUnconsumed` pack " 122350"
-- TODO Not sure exactly what to do with this yet. On Unix the whole thing
--      could be a file name, while on windows it is obviously an absolute path
--                 it "File name from absolute unix path" $
--                     ("c:\\Users\\user\\filename.txt" ~> path
--                         `shouldParse` $(mkRelFile "filename.txt")
      describe "[FAILURE]" $ do
        it "ASCII filename with space" $
          pack "file name.txt" ~?> path
            `leavesUnconsumed` pack " name.txt"
        it "Not at beginning of stream" $
          path `shouldFailOn` pack " filename"

    describe "Token" $
      describe "[SUCCESS]" $
        it "parse . encode == id" $ property $ \t ->
          tokenToBS t ~> token `parseSatisfies` (== t)

    describe "TCP port" $ do
      describe "[SUCCESS]" $
        it "parse . encode == id" $ property $ \p ->
          tcpPortToBS p ~> tcpPort `parseSatisfies` (== p)
      describe "[FAILURE]" $ do
        it "Parse <= 0" $
          forAll (choose (minBound, 0 :: Int)) $ \p ->
             tcpPort `shouldFailOn` pack (show p)
        it "Parse > 65535" $
          forAll (choose (65535, maxBound :: Int)) $ \p ->
             tcpPort `shouldFailOn` pack (show p)

    describe "File offset" $ do
      describe "[SUCCESS]" $
        it "parse . encode == id" $ property $ \o ->
          fileOffsetToBS o ~> fileOffset `parseSatisfies` (== o)
      describe "[FAILURE]" $
        it "Parse < 0" $
          forAll (choose (minBound, -1 :: Int)) $ \o ->
             fileOffset `shouldFailOn` pack (show o)

  describe "DCC commands" $ do

    describe "DCC CHAT" $
      describe "[SUCCESS]" $
        it "fromCtcp . toCtcp == id" $ property $ \cmd ->
          fromCtcp (toCtcp cmd) == Right (cmd :: DccChat)

    describe "DCC CLOSE" $
      describe "[SUCCESS]" $
        it "fromCtcp . toCtcp == id" $
          fromCtcp (toCtcp Close) == Right Close

    describe "DCC SEND" $
      describe "[SUCCESS]" $
        it "fromCtcp . toCtcp == id" $ property $ \cmd ->
          fromCtcp (toCtcp cmd) == Right (cmd :: DccSend)

    describe "DCC RESUME" $
      describe "[SUCCESS]" $
        it "fromCtcp . toCtcp == id" $ property $ \cmd ->
          fromCtcp (toCtcp cmd) == Right (cmd :: DccResume)

    describe "DCC ACCEPT" $
      describe "[SUCCESS]" $
        it "fromCtcp . toCtcp == id" $ property $ \cmd ->
          fromCtcp (toCtcp cmd) == Right (cmd :: DccAccept)

    describe "DCC SEND (reverse client)" $
      describe "[SUCCESS]" $
        it "fromCtcp . toCtcp == id" $ property $ \cmd ->
          fromCtcp (toCtcp cmd) == Right (cmd :: DccSendReverseClient)
