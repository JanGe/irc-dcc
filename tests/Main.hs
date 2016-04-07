module Main where

import qualified Network.IRC.DCCTest as DCC
import           Test.Tasty

main :: IO ()
main = DCC.spec >>= defaultMain
