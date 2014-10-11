{-# LANGUAGE TemplateHaskell
           , OverloadedStrings #-}

import Bvldr.Core.Types
import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Control.Monad (mzero)
import Data.Aeson ((.:), FromJSON (..), Value (..), eitherDecode)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Types.Method (methodPost)
import Network.Wai.Handler.Warp (run)
import Restmachine.Core.Types
import Restmachine.Wai (wrap)

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Types.Status as H

notFound = defaultResource & (response .~ (static $ Response H.status404 [] "not found"))

githubCallback = defaultResource & (knownMethod .~ (\req -> return $ req ^. requestMethod == methodPost))
                                 & (response .~ (\req -> do
                                    let mcommit = eitherDecode $ req ^. body :: Either String GithubEvent
                                    putStrLn $ show mcommit
                                    return $ Response H.status200 [] $ req ^. body))

app = Application [(["_callbacks", "github"], githubCallback)] notFound

main = run 3000 $ wrap app