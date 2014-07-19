{-# LANGUAGE OverloadedStrings #-}
import Container.Docker (create, defaultContainerRequest)
import Container.Docker.Types

import Control.Lens

import qualified Data.Text.IO as T

main :: IO ()
main = do
  mid <- create ctx defaultContainerRequest
  case mid of
    Just id -> T.putStrLn id
    _       -> T.putStrLn "Error creating a container"

  where

  ctx = DefaultDockerContext "http://localhost:1234"
  req = defaultContainerRequest & cmd .~ ["sleep", "5"]