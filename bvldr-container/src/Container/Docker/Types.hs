{-# LANGUAGE TemplateHaskell
           , OverloadedStrings #-}
module Container.Docker.Types where


import Data.Text (Text)

type Image       = Text
type Command     = Text
type ContainerID = Text

class DockerContext a where
  
  -- | The baseurl of the docker server.
  baseurl :: a -> String

data DefaultDockerContext = DefaultDockerContext {
    _baseurl              :: String
  }
  deriving (Show)

instance DockerContext DefaultDockerContext where
  baseurl = _baseurl
  