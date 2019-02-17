{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards, DuplicateRecordFields #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (unpackChars)
import           Data.Aeson (encode, decode, ToJSON, FromJSON)
import           Data.Map.Syntax ((##))
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
-- import           Snap.Snaplet.PostgresqlSimple
-- import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Util.FileServe
import           Text.Read (readMaybe)
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application
import           Data

------------------------------------------------------------------------------

pathParam :: (MonadSnap m, Read a) => ByteString -> m a
pathParam name = do
    val <- getParam name
    case unpackChars <$> val >>= readMaybe of
        Just a -> return a
        Nothing -> getResponse >>= finishWith . setResponseStatus 400 ("Bad/missing path parameter: " <> name)

bodyJson :: (MonadSnap m, FromJSON a) => m a
bodyJson = do
  body <- readRequestBody 50000 -- magic number for max body length
  case decode body of
      Just a -> return a
      Nothing -> getResponse >>= finishWith . setResponseStatus 400 "Malformed JSON body"

jsonResponse :: (MonadSnap m, ToJSON a) => a -> m ()
jsonResponse a = do
    writeLBS $ encode a
    modifyResponse $ setHeader "Content-Type" "application/json"

type Endpoint = Handler App (AuthManager App) ()

------------------------------------------------------------------------------
handleAttemptLogin :: Endpoint
handleAttemptLogin = do
  HTTPLogin{..} <- bodyJson
  let dummy = HTTPUser{id = 0, name = "", email = "", authenticated = True, ..}
  jsonResponse dummy


------------------------------------------------------------------------------
handleLogout :: Endpoint
handleLogout = do
  HTTPUser{..} <- bodyJson
  let dummy = HTTPUser{authenticated = False, ..}
  jsonResponse dummy


------------------------------------------------------------------------------
handleCreateUser :: Endpoint
handleCreateUser = do
  HTTPCreateUser{..} <- bodyJson
  let dummy = HTTPNewUser{id = Just 0, name = fullname, authenticated = False, ..}
  jsonResponse dummy


------------------------------------------------------------------------------
handleCreateDebate :: Endpoint
handleCreateDebate = do
  HTTPCreateDebate{..} <- bodyJson
  let dummy = HTTPNewDebate{id = Just 0, ..}
  jsonResponse dummy


------------------------------------------------------------------------------
handleDebateList :: Endpoint
handleDebateList = do
  let dummy = [] :: [HTTPDebate]
  jsonResponse dummy


------------------------------------------------------------------------------
handleDebate :: Endpoint
handleDebate = do
  let dummy = Nothing :: Maybe HTTPDebate
  jsonResponse dummy


------------------------------------------------------------------------------
handleOpinionPair :: Endpoint
handleOpinionPair = do
  let dummy = Just [ HTTPOpinion{id = 0, authorId = 0, description = "foo", ranking = 0}
                   , HTTPOpinion{id = 1, authorId = 1, description = "bar", ranking = 1}
                   ]
  jsonResponse dummy


------------------------------------------------------------------------------
handleVote :: Endpoint
handleVote = do
  let dummy = True
  jsonResponse dummy


------------------------------------------------------------------------------
handleOpine :: Endpoint
handleOpine = do
  let dummy = True
  jsonResponse dummy


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = fmap (with auth) <$>
         [ ("attempt-login",                post handleAttemptLogin)
         , ("logout",                       post handleLogout)
         , ("create-user",                  post handleCreateUser)
         , ("create-debate",                post handleCreateDebate)
         , ("debate-list",                   get handleDebateList)
         , ("debate/:debate",                get handleDebate)
         , ("opinion-pair/:debateId",        get handleOpinionPair)
         , ("vote",                         post handleVote)
         , ("opine",                        post handleOpine)
         ]
         where post = method POST
               get  = method GET


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)
    -- d <- nestSnaplet "db" db $ pgsInit' $ pgsDefaultConfig "host=localhost port=5432 dbname=debate"
    -- a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a --d
