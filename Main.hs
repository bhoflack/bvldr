{-# LANGUAGE TemplateHaskell
           , OverloadedStrings #-}

import Bvldr.Core.Types
import Control.Applicative ((<$>), (<*>))
import Control.Lens
import Control.Monad (mapM, mzero)
import Data.Aeson ((.:), FromJSON (..), Value (..), eitherDecode)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Types.Method (methodPost)
import Network.Wai.Handler.Warp (run)
import Restmachine.Core.Types
import Restmachine.Wai (wrap)
import System.Process.ByteString (readProcessWithExitCode)
import System.Exit (ExitCode (..))

import Prelude hiding (id)

import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as T
import qualified Network.HTTP.Types.Status as H

notFound = defaultResource & (response .~ (static $ Response H.status404 [] "not found"))

githubCallback = defaultResource & (knownMethod .~ (\req -> return $ req ^. requestMethod == methodPost))
                                 & (response .~ (\req -> do
                                    let mcommit = eitherDecode $ req ^. body :: Either String GithubEvent
                                    case mcommit of
                                      Right p -> do
                                        let r = (p ^. repository)
                                            cs = p ^. commits
                                            refs = map (\c -> c ^. id) cs
                                        Just () <- clone r
                                        r <- mapM build refs
                                        return $ Response H.status200 [] $ "ok"
                                      Left e -> return $ Response H.status500 [] $ BL8.pack e))

clone :: Repository -> IO (Maybe ())
clone rep = do
  (code, stdout, stderr) <- readProcessWithExitCode "/usr/local/bin/git" ["clone", T.unpack url] B8.empty
  case code of
    ExitSuccess -> return $ Just ()
    _ -> do
      B8.putStrLn stderr
      return Nothing
  where
  url = rep ^. sshUrl

build :: Text -> IO (Maybe ())
build ref = do
  (code, stdout, stderr) <- readProcessWithExitCode "/usr/local/bin/git" ["--git-dir", "calculator", "checkout", T.unpack ref] B8.empty
  case code of
    ExitSuccess -> 
      return $ Just ()
    _ -> 
      B8.putStrLn stderr >> 
      return Nothing

app = Application [(["_callbacks", "github"], githubCallback)] notFound

main = run 3000 $ wrap app