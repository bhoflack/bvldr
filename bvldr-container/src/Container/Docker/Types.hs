{-# LANGUAGE TemplateHaskell
           , OverloadedStrings #-}
module Container.Docker.Types where

import Control.Lens (makeLenses, (^.))
import Control.Monad ((>=>)) 

import Data.Aeson (object, ToJSON (..), Value (..), (.=))
import Data.Map (Map)
import Data.Text (Text)

type Image = Text
type Command = Text
type ContainerID = Text

data CreateContainerReq = 
  CreateContainerReq { _hostname        :: Maybe Text,
                       _user            :: Maybe Text,
                       _memory          :: Maybe Integer,
                       _attachStdin     :: Bool,
                       _attachStdout    :: Bool,
                       _attachStderr    :: Bool,
                       _portSpecs       :: Maybe Text,
                       _tty             :: Bool,
                       _openStdin       :: Bool,
                       _stdinOnce       :: Bool,
                       _env             :: Maybe (Map Text Text),
                       _cmd             :: [Text],
                       _image           :: Image,
                       _volumes         :: Maybe (Map Text Text),
                       _workingDir      :: FilePath,
                       _disableNetwork  :: Bool,
                       _exposedPorts    :: Maybe (Map Text Text)
                       }
  deriving (Show, Eq)
makeLenses ''CreateContainerReq

instance ToJSON CreateContainerReq where
  toJSON req = 
    let orDefault d = maybe d id in
    object [ "Hostname"              .= (orDefault "" $ req ^. hostname)
           , "User"                  .= (orDefault "" $ req ^. user)
           , "Memory"                .= (orDefault 0 $ req ^. memory)
           , "AttachStdin"           .= (req ^. attachStdin)
           , "AttachStdout"          .= (req ^. attachStdout)
           , "AttachStderr"          .= (req ^. attachStderr)
           , "PortSpecs"             .= (req ^. portSpecs)
           , "Tty"                   .= (req ^. tty)
           , "OpenStdin"             .= (req ^. openStdin)
           , "StdinOnce"             .= (req ^. stdinOnce)
           , "Env"                   .= (req ^. env)
           , "Cmd"                   .= (req ^. cmd)
           , "Image"                 .= (req ^. image)
           , "Volumes"               .= (req ^. volumes)
           , "WorkingDir"            .= (req ^. workingDir)
           , "disableNetwork"        .= (req ^. disableNetwork)
           , "exposedPorts"          .= (req ^. exposedPorts)
           ]