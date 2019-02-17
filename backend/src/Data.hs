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

data HTTPNewUser = HTTPNewUser
    { id :: Maybe Id,
      username :: Text,
      name :: Text,
      email :: Text,
      authenticated :: Bool
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data HTTPUser = HTTPUser
    { id :: Id
    , username :: Text
    , name :: Text
    , email :: Text
    , authenticated :: Bool
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data HTTPCreateDebate = HTTPCreateDebate
    { title :: Text
    , imageUrl :: Text
    , subtitle :: Text
    , description :: Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data HTTPNewDebate = HTTPNewDebate
    { id :: Maybe Id
    , title :: Text
    , imageUrl :: Text
    , subtitle :: Text
    , description :: Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SQLDebate = SQLDebate
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
    { debate :: Id
    , uid :: Id
    , description :: Text
    , author :: Id
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Vote = Vote
    { uid :: Id
    , voter :: Id
    , debate :: Id
    , option1 :: Id
    , option2 :: Id
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)
