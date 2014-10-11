{-# LANGUAGE TemplateHaskell
           , OverloadedStrings #-}
module Bvldr.Core.Types where

import Control.Applicative ((<$>), (<*>))
import Control.Lens (makeLenses)
import Control.Monad (mzero)
import Data.Aeson ((.:), FromJSON (..), Value (..))
import Data.Text (Text)


data User = User
  { _name                   :: Text
  , _email                  :: Text
  , _username               :: Text
  } deriving (Show, Eq)

data Commit = Commit
  { _id                 :: Text
  , _distinct           :: Bool
  , _message            :: Text
  , _url                :: Text
  , _author             :: User
  , _committer          :: User
  , _added              :: [Text]
  , _removed            :: [Text]
  , _modified           :: [Text]
  } deriving (Show, Eq)

data GithubEvent = PushEvent
  { _ref        :: Text
  , _before     :: Text
  , _after      :: Text
  , _created    :: Bool
  , _deleted    :: Bool
  , _forced     :: Bool
  , _commits    :: [Commit]
  } deriving (Show, Eq)

makeLenses ''User
makeLenses ''Commit
makeLenses ''GithubEvent

instance FromJSON User where
  parseJSON (Object v) = User <$>
                              v .: "name" <*>
                              v .: "email" <*>
                              v .: "username"

  parseJSON _ = mzero

instance FromJSON Commit where
  parseJSON (Object v) = Commit <$>
                                v .: "id" <*>
                                v .: "distinct" <*>
                                v .: "message" <*>
                                v .: "url" <*>
                                v .: "author" <*>
                                v .: "committer" <*>
                                v .: "added" <*>
                                v .: "removed" <*>
                                v .: "modified"

  parseJSON _ = mzero

instance FromJSON GithubEvent where
  parseJSON (Object v) = PushEvent <$>
                                   v .: "ref" <*>
                                   v .: "before" <*>
                                   v .: "after" <*>
                                   v .: "created" <*>
                                   v .: "deleted" <*>
                                   v .: "forced" <*>
                                   v .: "commits"

  parseJSON _ = mzero
