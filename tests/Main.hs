module Main where

import qualified Network.IRC.DCCTest as DCC
import           Test.Tasty
import           Test.Tasty.Hspec

main = tests

tests :: IO ()
tests = DCC.spec >>= defaultMain
