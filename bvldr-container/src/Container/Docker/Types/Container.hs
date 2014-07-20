{-# LANGUAGE TemplateHaskell
           , OverloadedStrings #-}
module Container.Docker.Types.Container where

import Control.Applicative ((<$>), (<*>))
import Control.Lens (makeLenses, (^.))

import Data.Aeson (FromJSON (..), Value (..), (.:))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

data Container =
  Container { _id               :: Text
            , _image            :: Text
            , _command          :: Text
            , _created          :: UTCTime
            , _status           :: Text
            , _sizeRw           :: Integer
            , _sizeRootFs       :: Integer
            }
  deriving (Show, Eq)
makeLenses ''Container

instance FromJSON Container where
  parseJSON (Object v) = Container <$>
                           v .: "Id" <*>
                           v .: "Image" <*>
                           v .: "Command" <*>
                           v .: "Created" <*>
                           v .: "Status" <*>
                           v .: "SizeRw" <*>
                           v .: "SizeRootFs"
