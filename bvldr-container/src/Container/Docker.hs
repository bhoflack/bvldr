{-# LANGUAGE OverloadedStrings #-}
module Container.Docker
  ( create ) where

import Container.Docker.Types (Image, Command, ContainerID)

import Data.Text (Text)

create :: Image -> Command -> IO (Maybe ContainerID)
create i c = return $ Just "hello"