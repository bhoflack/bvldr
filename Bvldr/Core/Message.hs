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
  | AddCustomer CustomerID
  | RemoveCustomer CustomerID
  | AddCommitRef CommitID
  | BuildGitCommit CommitID
  deriving (Show, Eq)