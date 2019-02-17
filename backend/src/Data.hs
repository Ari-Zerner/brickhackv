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

data OpinionNew = OpinionNew
    { text :: T.Text
    , author :: Id
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Voting = Voting
    { votingId :: Id
    , options :: [Id]
    , subject :: Id
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

