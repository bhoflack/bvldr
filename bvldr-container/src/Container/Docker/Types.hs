module Container.Docker.Types
  (

  ) where

import Data.Map (Map)
import Data.Text (Text)

type Image = Text
type Command = Text
type ContainerID = Text

data CreateContainerReq = 
  CreateContainerReq { _hostname        :: Maybe Text,
                       _user            :: Maybe Text,
                       _memory          :: Maybe Integer,
                       _attachStdin     :: Boolean,
                       _attachStdout    :: Boolean,
                       _attachStderr    :: Boolean,
                       _portSpecs       :: Maybe Text,
                       _tty             :: Boolean,
                       _openStdin       :: Boolean,
                       _stdinOnce       :: Boolean,
                       _env             :: Maybe (Map Text Text),
                       _cmd             :: [Text],
                       _image           :: Image,
                       _volumes         :: Maybe (Map Text Text),
                       _workingDir      :: Path,
                       _disableNetwork  :: Boolean,
                       _exposedPorts    :: Maybe (Map Text Text)
                       }
  deriving (Show, Eq, Lens)

instance ToJSON CreateContainerReq where
  toJSON req = object [ "Hostname" .= req ^. hostname . _String
                      , "User"
