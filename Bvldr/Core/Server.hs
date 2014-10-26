-- | The server is waiting for messages to be sent and then coordinates the executors.
-- Supported messages are:
-- - AddCommitRef add a commit ref to the server.  This will trigger it to build on one
--   of its nodes.

module Bvldr.Core.Server

  where

import System.IO (hClose)
import System.Process (createProcess, shell, waitForProcess)

data Message = AddCommitRef
  { _ref      :: Text
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
     hClose stdin
     hClose stdout
     hClose stderr
     