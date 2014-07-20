{-# LANGUAGE OverloadedStrings #-}
import Container.Docker (create, list, defaultContainerRequest)
import Container.Docker.Types
import Container.Docker.Types.CreateContainerReq

import Control.Lens

import qualified Data.Text.IO as T
import System.Environment (getArgs)

main :: IO ()
main = do
  [baseuri] <- getArgs

  let ctx = DefaultDockerContext baseuri
      req = defaultContainerRequest & cmd .~ ["sleep", "5"]
  
  mid <- create ctx req
  case mid of
    Just id -> do
      T.putStrLn id
      mContainers <- list ctx
      putStrLn (show mContainers)
    _       -> T.putStrLn "Error creating a container"


