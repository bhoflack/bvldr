{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Network.HTTP.Types.Method (methodPost)
import Network.Wai.Handler.Warp (run)
import Restmachine.Core.Types
import Restmachine.Wai (wrap)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Network.HTTP.Types.Status as H

notFound = defaultResource & (serviceAvailable .~ static True)
                           & (knownMethod .~ static True)
                           & (methodAllowed .~ static True)
                           & (forbidden .~ static False)
                           & (response .~ (static $ Response H.status404 [] "not found"))

githubCallback = defaultResource & (serviceAvailable .~ static True)
                                 & (knownMethod .~ (\req -> return $ req ^. requestMethod == methodPost))
                                 & (methodAllowed .~ static True)
                                 & (forbidden .~ static False)
                                 & (response .~ (\req -> do
                                    BS.putStrLn $ req ^. body
                                    return $ Response H.status200 [] $ BSL.fromStrict $ req ^. body))

app = Application [(["_callbacks", "github"], githubCallback)] notFound

main = run 3000 $ wrap app