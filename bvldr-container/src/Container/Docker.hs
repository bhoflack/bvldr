{-# LANGUAGE OverloadedStrings #-}
module Container.Docker
  ( create ) where

import Data.Text (Text)

type Image = Text
type Command = Text
type ContainerID = Text

create :: Image -> Command -> IO (Maybe ContainerID)
create i c = do
