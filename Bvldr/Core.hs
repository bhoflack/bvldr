{-# LANGUAGE TemplateHaskell #-}
module Bvldr.Core where

import Bvldr.Core.Message (CustomerID, Message (..), NodeID)
import Control.Lens (over, makeLenses, (^.), set)
import Control.Monad.State (State (..), modify, get, put)

data Node = InitialNode { _nodeId :: NodeID }
          | AvailableNode { _nodeId :: NodeID }
          | BusyNode { _nodeId :: NodeID }
  deriving (Show, Eq)

data Customer = Customer { _customerId :: CustomerID }
  deriving (Show, Eq)

data Server = Server { _nodes     :: [Node]
                     , _customers :: [Customer]
                     , _jobs      :: [Message]
                     }
  deriving (Show, Eq)

data Command = Command { _commandNodeId         :: NodeID
                       , _commandMessage        :: Message
                       }
  deriving (Show, Eq)

makeLenses ''Node
makeLenses ''Customer
makeLenses ''Server
makeLenses ''Command

run :: Message -> State Server [Command]
run (AddBuildNode n) = do
  modify (over nodes $ (:) (InitialNode n))
  return [Command n Ping]
run (RemoveBuildNode i) = do
  modify (over nodes $ filter (\n -> n^.nodeId /= i))
  return []
run (AddCustomer n) = do
  modify (over customers $ (:) (Customer n))
  return []
run (RemoveCustomer n) = do
  modify (over customers $ filter (\c -> c^.customerId /= n))
  return []
run m@(AddCommitRef r) = do
  s <- get
  let ns = s^.nodes
      availableNodes = filter isAvailableNode ns
  if availableNodes == []
    then do
         modify (over jobs $ (:) m)
         return []
    else do
         let n = head availableNodes
             bn = BusyNode (n^.nodeId)
             ns' = filter (\n -> n^.nodeId /= n^.nodeId) ns
             cm = BuildGitCommit r
         modify (set nodes $ bn : ns')
         return [Command (bn^.nodeId) cm]

isAvailableNode :: Node -> Bool
isAvailableNode (AvailableNode _) = True
isAvailableNode _ = False