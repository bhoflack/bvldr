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
  , _pusher     :: Pusher
  , _repository :: Repository
  } deriving (Show, Eq)

data Pusher = Pusher
  { _pusherName         :: Text
  , _pusherEmail        :: Text
  } deriving (Show, Eq)

data Repository = Repository
  { _gitUrl     :: Text
  , _sshUrl     :: Text
  , _cloneUrl   :: Text
  } deriving (Show, Eq)

makeLenses ''User
makeLenses ''Commit
makeLenses ''GithubEvent
makeLenses ''Pusher
makeLenses ''Repository

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
                                   v .: "commits" <*>
                                   v .: "pusher" <*>
                                   v .: "repository"

  parseJSON _ = mzero

instance FromJSON Pusher where
  parseJSON (Object v) = Pusher <$>
                                v .: "name" <*>
                                v .: "email"
  parseJSON _ = mzero

instance FromJSON Repository where
  parseJSON (Object v) = Repository <$> 
                                   v .: "git_url" <*>
                                   v .: "ssh_url" <*>
                                   v .: "clone_url"
  parseJSON _ = mzero