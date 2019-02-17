{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass, DuplicateRecordFields #-}

module Data where

import qualified Data.Text as T
import GHC.Generics
import Data.Aeson

type Id = Int

data Subject = Subject
    { uid :: Id
    , name :: T.Text
    , description :: T.Text
    , author :: Id
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data User = User
    { uid :: Id
    , username :: T.Text
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Opinion = Opinion
    { subject :: Id
    , uid :: Id
    , description :: T.Text
    , author :: Id
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

data Vote = Vote
    { uid :: Id
    , voter :: Id
    , subject :: Id
    , option1 :: Id
    , option2 :: Id
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)
