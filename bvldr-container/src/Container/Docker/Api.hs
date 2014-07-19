module Container.Docker.Api
  (
    post
  ) where

import Container.Docker.Types (DockerContext (..))

import Data.Aeson (ToJSON (..))
import Data.ByteString.Lazy (ByteString)

import Network.Wreq (Response)

import qualified Network.Wreq as Wreq

-- | Post a JSON message to the docker server.
-- The `msg' is converted and sent to the endpoint 
-- at `uri'.  
-- The `Response' is returned as a lazy ByteStream.
post :: (DockerContext ctx, ToJSON a)
     => ctx
     -> String
     -> a
     -> IO (Response ByteString)
post ctx uri msg = Wreq.post url body
  where
  url = baseurl ctx ++ uri
  body = toJSON msg