module Bvldr.Core.Server

  where

import System.Process (createProcess, shell, waitForProcess)

data Message = AddCommitRef { _ref      :: Text
                            }

data Node = SimpleNode

data Server = Server 
  { _messages        :: [Message]
  , _nodes           :: [Node]
  }

operate :: Server -> Message -> IO ()
operate server msg = case msg of
  AddCommitRef r -> do
     (stdin, stdout, stderr, ph) <- createProcess $ shell "./.builder.sh"
     waitForProcess ph
     