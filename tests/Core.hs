{-# LANGUAGE OverloadedStrings #-}
module Core where

import Bvldr.Core
import Bvldr.Core.Message (Message (..))

import Control.Lens ((^.))
import Control.Monad.State (runState)

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, (~=?))

tests :: [Test]
tests = [
      testGroup "Node management" [
      testCase "Add a node" addNode
    , testCase "Remove a node" removeNode
    ]

    , testGroup "Customer" [
      testCase "Add a customer" addCustomer
    ]

    , testGroup "Commits" [
      testCase "Queue a commit ref" queueCommitRef
    , testCase "Forward a commit ref to a build node" forwardCommitRef
    ]
  ]

addNode :: Assertion
addNode = do 
  let r = runState (run msg) initialServer
  assertEqual "The server should have the node appended" ([Command "test 123" Ping], expectedServer) r
  where
  msg = AddBuildNode "test 123"
  initialServer = Server [] [] [] []
  expectedServer = Server [InitialNode "test 123"] [] [] []

removeNode :: Assertion
removeNode = do
  let r = runState (run msg) initialServer
  assertEqual "The server should have the node removed" ([], expectedServer) r
  where
  msg = RemoveBuildNode "abc123"
  initialServer = Server [AvailableNode "abc456", BusyNode "abc123" Ping] [] [] []
  expectedServer = Server [AvailableNode "abc456"] [] [] []

addCustomer :: Assertion
addCustomer = do
  let (_, s) = runState (run msg) initialServer
  assertEqual "The server should keep track of the new customer" [Customer "abc" "key"] (s^.customers)
  where
  msg = AddCustomer "abc" "key"
  initialServer = Server [] [] [] []

removeCustomer :: Assertion
removeCustomer = do
  let (_, s) = runState (run msg) initialServer
  assertEqual "The server should remove the customer" [Customer "def" "key"] (s^.customers)
  where
  msg = RemoveCustomer "abc"
  initialServer = Server [] [] [Customer "abc" "key", Customer "def" "key"] []

queueCommitRef :: Assertion
queueCommitRef = do
  let r = runState (run msg) initialServer
  assertEqual "The server should queue the commits when no build node is available" ([], expectedServer) r
  where
  msg = AddCommitRef "b3123" "cloneurl"
  initialServer = Server [] [] [] []
  expectedServer = Server [] [] [] [msg]

forwardCommitRef :: Assertion
forwardCommitRef = do
  let r = runState (run msg) initialServer
  assertEqual "The server forwards a commit ref to an available node" ([Command "abc123" buildMsg], expectedServer) r
  where
  cloneUrl = "git@github.com:foo/bar.git"
  msg = AddCommitRef "b3123" cloneUrl
  privateKey = "pkey"
  buildMsg = BuildGitCommit "b3123" cloneUrl privateKey
  customer = Customer "abc" privateKey
  initialServer = Server [AvailableNode "abc123"] [Repository cloneUrl customer]  [customer] []
  expectedServer = Server [BusyNode "abc123" buildMsg] [Repository cloneUrl customer] [customer] []
