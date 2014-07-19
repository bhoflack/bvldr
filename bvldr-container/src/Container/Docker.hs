{-# LANGUAGE OverloadedStrings #-}
module Container.Docker
  ( create
  , defaultContainerRequest ) where

import qualified Container.Docker.Api as Api
import Container.Docker.Types (Image, Command, ContainerID, CreateContainerReq (..), DockerContext)

import Control.Lens ((^?))
import Data.Aeson (toJSON)
import Data.Aeson.Lens (key, _String)
import Data.Text (Text)
import Network.Wreq (responseBody)

-- | Create a `Container' on the docker server.
create :: (DockerContext c) => c -> CreateContainerReq -> IO (Maybe Text)
create c r = do
  resp <- Api.post c "/containers/create" r' 
  return $ resp ^? responseBody . key "Id" . _String 

  where

  r' = toJSON r

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
                     

                                                