module Container.Docker.Api
  ( get
  , post
  ) where

import Container.Docker.Types (DockerContext (..))

import Control.Lens ((^.))

import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.ByteString.Lazy (ByteString)

import Network.Wreq (Response, asJSON)

import qualified Network.Wreq as Wreq

-- | Get a object from the docker server.
get :: (DockerContext ctx)
    => ctx
    -> String
    -> IO (Response ByteString)
get ctx uri = Wreq.get url
  where
  url = baseurl ctx ++ uri

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
