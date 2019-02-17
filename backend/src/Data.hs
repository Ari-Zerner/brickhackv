{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, DuplicateRecordFields #-}

module Data where

import qualified Data.Text as T
import GHC.Generics
import Data.Aeson
import Snap.Snaplet.PostgresqlSimple

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

data HTTPOpinion = HTTPOpinion
    { id :: Id
    , authorId :: Id
    , description :: Text
    , ranking :: Double
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data HTTPDebate = HTTPDebate
    { id :: Id
    , title :: Text
    , imageUrl :: Text
    , subtitle :: Text
    , description :: Text
    , viewCount :: Int
    , opinionCount :: Int
    , bookmarked :: Bool
    , opined :: Bool
    , voted :: Bool
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data HTTPDebateDetail = HTTPDebateDetail
    { id :: Id
    , title :: Text
    , imageUrl :: Text
    , subtitle :: Text
    , description :: Text
    , viewCount :: Int
    , opinionCount :: Int
    , bookmarked :: Bool
    , opined :: Bool
    , voted :: Bool
    , opinions :: [HTTPOpinion]
    , myOpinion :: Maybe HTTPOpinion
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SQLDebate = SQLDebate
    { uid :: Id
    , name :: Text
    , description :: Text
    , author :: Id
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToRow, FromRow)

data SQLUser = SQLUser
    { uid :: Id
    , username :: Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToRow, FromRow)

data SQLOpinion = SQLOpinion
    { debate :: Id
    , uid :: Id
    , description :: Text
    , author :: Id
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToRow, FromRow)

data SQLVote = SQLVote
    { uid :: Id
    , voter :: Id
    , debate :: Id
    , winner :: Id
    , loser :: Id
    } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToRow, FromRow)
