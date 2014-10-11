{-# LANGUAGE OverloadedStrings #-}
module Json where

import Bvldr.Core.Types

import Data.Aeson (decode)

import System.IO (IOMode (..), withFile)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual)

import qualified Data.ByteString.Lazy.Char8 as BSL8

tests :: [Test]
tests = [
    testGroup "pushEvent" [
      testCase "decode a push event" decodePushEvent
    ]
  ]

decodePushEvent :: Assertion
decodePushEvent = withFile "push.json" ReadMode $ \h -> do
  js <- BSL8.hGetContents h
  let mev = decode js :: Maybe GithubEvent
  assertEqual "" mev (Just $ PushEvent "refs/heads/master"
                                       "393f74b7cc35846dafea38266f5bbbd13f174b3a"
                                       "15681f752384bd3444250b4fc3c450db9ca69990" False False True 
                                       [Commit "15681f752384bd3444250b4fc3c450db9ca69990"
                                               True "Use query param to override storage cookie"
                                               "https://github.com/foo/bar/commit/15681f752384bd3444250b4fc3c450db9ca69990"
                                               (User "Tim Schaub" "tim.schaub@example.com" "tschaub")
                                               (User "Tim Schaub" "tim.schaub@example.com" "tschaub")
                                               [] [] ["src/common/loader.js"]])