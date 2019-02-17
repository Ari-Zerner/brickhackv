{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, DuplicateRecordFields #-}

module Data where

import qualified Data.Text as T
import GHC.Generics
import Data.Aeson

type Id = Int
type Text = T.Text

data HTTPLogin = HTTPLogin
    { username :: Text
    , password :: Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data HTTPCreateUser = HTTPCreateUser
    { username :: Text,
		  fullname :: Text,
		  email :: Text,
      password :: String
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data HTTPUser = HTTPUser
    { id :: Id,
		  username :: Text,
		  name :: Text,
		  email :: Text,
		  authenticated :: Bool
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Subject = Subject
    { uid :: Id
    , name :: Text
    , description :: Text
    , author :: Id
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data User = User
    { uid :: Id
    , username :: Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Opinion = Opinion
    { subject :: Id
    , uid :: Id
    , description :: Text
    , author :: Id
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Vote = Vote
    { uid :: Id
    , voter :: Id
    , subject :: Id
    , option1 :: Id
    , option2 :: Id
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)
