{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, DuplicateRecordFields #-}

module Data where

import qualified Data.Text as T
import GHC.Generics
import Data.Aeson

type Id = Int

data Subject = Subject
    { subjectId :: Id
    , name :: T.Text
    , description :: T.Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Opinion = Opinion
    { opinionId :: Id
    , text :: T.Text
    , author :: Id
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Vote = Vote
    { votingId :: Id
    , options :: [Opinion]
    , subject :: Id
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)
