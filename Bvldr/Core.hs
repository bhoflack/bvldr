{-# LANGUAGE TemplateHaskell #-}
module Bvldr.Core where

import Bvldr.Core.Message (CustomerID, Message (..), NodeID)
import Control.Lens (over, makeLenses, (^.), set)
import Control.Monad.State (State (..), modify, get, put)

import Data.Text (Text)

data Node = InitialNode { _nodeId :: NodeID }
          | AvailableNode { _nodeId :: NodeID }
          | BusyNode { _nodeId :: NodeID, _command :: Message }
  deriving (Show, Eq)

data Customer = Customer { _customerId :: CustomerID, _customerPrivateKey :: Text }
  deriving (Show, Eq)

data Repository = Repository { _repCloneUrl        :: Text
                             , _repCustomer        :: Customer
                             }
  deriving (Show, Eq)

data Server = Server { _nodes           :: [Node]
                     , _repositories    :: [Repository]
                     , _customers       :: [Customer]
                     , _jobs            :: [Message]
                     }
  deriving (Show, Eq)

data Command = Command { _commandNodeId         :: NodeID
                       , _commandMessage        :: Message
                       }
  deriving (Show, Eq)

makeLenses ''Node
makeLenses ''Repository
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
run (AddCustomer n pkey) = do
  modify (over customers $ (:) (Customer n pkey))
  return []
run (RemoveCustomer n) = do
  modify (over customers $ filter (\c -> c^.customerId /= n))
  return []
run m@(AddCommitRef ref cloneUrl) = do
  s <- get
  let ns = s^.nodes

  case find isAvailableNode ns of
    Just n -> do 
      case find (\r -> r^.repCloneUrl == cloneUrl) (s^.repositories) of
        Just r -> do
          let pk = r^.repCustomer.customerPrivateKey
              cm = BuildGitCommit ref cloneUrl pk
              bn = BusyNode (n^.nodeId) cm
              ns' = filter (\n -> n^.nodeId /= n^.nodeId) ns
          modify (set nodes $ bn : ns')
          return [Command (bn^.nodeId) cm]
        Nothing -> return []
    Nothing -> do
      modify (over jobs $ (:) m)
      return []

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x:xs) = if p x 
                  then Just x
                  else find p xs

isAvailableNode :: Node -> Bool
isAvailableNode (AvailableNode _) = True
isAvailableNode _ = False