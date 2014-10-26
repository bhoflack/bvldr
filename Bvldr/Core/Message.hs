module Bvldr.Core.Message
  ( CustomerID
  , Message (..)
  , NodeID
  )
  where

import Data.Text (Text)

type NodeID = Text
type CustomerID = Text
type CommitID = Text

-- | Messages supported by the Core.
data Message = 
    AddBuildNode NodeID
  | RemoveBuildNode NodeID
  | Ping
  | Pong
  | AddCustomer { customerId    :: CustomerID
                , privateKey    :: Text
                }
  | RemoveCustomer CustomerID
  | AddCommitRef { commitId     :: CommitID
                 , cloneURL     :: Text
                 }
  | BuildGitCommit { commitId   :: CommitID
                   , cloneURL   :: Text
                   , privateKey :: Text
                   }
  deriving (Show, Eq)