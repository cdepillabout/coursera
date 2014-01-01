module Main ( main
            )
            where

--import Test.Tasty (defaultMain, testGroup, TestTree)
import qualified Test.Tasty as Tasty
import qualified Test.Framework as Framework
import qualified Test.Framework.Runners.Console as Framework.Runners.Console

import MyQuickCheck
import MyHUnit

main :: IO ()
--main = Tasty.defaultMain tastytests
main = Framework.Runners.Console.defaultMain [frameworktests]

tastytests :: Tasty.TestTree
tastytests = Tasty.testGroup "Tests" [tastyproperties]

frameworktests :: Framework.Test
frameworktests = Framework.testGroup "Tests"
        [ Framework.testGroup "QuickCheck" frameworkproperties
        , Framework.testGroup "HUnit" frameworkUnitTests
        ]
