module Main ( main
            )
            where

import Test.Framework (defaultMain, testGroup)

import QuickCheckTests
import HUnitTests

main :: IO ()
main = defaultMain [ testGroup "Quick Check Tests" quickCheckTests
                   , testGroup "HUnit Tests" hUnitTests
                   ]
