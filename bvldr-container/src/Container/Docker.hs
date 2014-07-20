{-# LANGUAGE OverloadedStrings #-}
module Container.Docker
  ( create
  , list
  , defaultContainerRequest 
  ) where

import qualified Container.Docker.Api as Api
import Container.Docker.Types (Image, Command, ContainerID, DockerContext)
import Container.Docker.Types.CreateContainerReq
import Container.Docker.Types.Container

import Control.Lens ((^?), (^.))
import Control.Monad ((=<<))
import Data.Aeson (decode, toJSON)
import Data.Aeson.Lens (key, _String)
import Data.Text (Text)
import Network.Wreq (Response, asJSON, responseBody)

-- | Create a `Container' on the docker server.
create :: (DockerContext c) => c -> CreateContainerReq -> IO (Maybe Text)
create c r = do
  resp <- Api.post c "/containers/create" r' 
  return $ resp ^? responseBody . key "Id" . _String 

  where

  r' = toJSON r

-- | List the `Container's on the docker server.
list :: (DockerContext c) => c -> IO (Maybe [Container])
list c = do
  r <- Api.get c "/containers/json"
  return $ decode $ r ^. responseBody

defaultContainerRequest = 
  CreateContainerReq Nothing
                     Nothing
                     Nothing
                     False
                     False
                     False
                     Nothing
                     False
                     False
                     False
                     Nothing
                     ["echo", "hello world"]
                     "ubuntu"
                     Nothing
                     "/"
                     False
                     Nothing
                     

                                                