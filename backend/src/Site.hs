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



------------------------------------------------------------------------------
handleAttemptLogin :: Handler App (AuthManager App) ()
handleAttemptLogin = do
  HTTPLogin{..} <- bodyJson
  let dummy = HTTPUser{id = 0, name = "", email = "", authenticated = True, ..}
  jsonResponse dummy


------------------------------------------------------------------------------
handleLogout :: Handler App (AuthManager App) ()
handleLogout = do
  HTTPUser{..} <- bodyJson
  let dummy = HTTPUser{authenticated = False, ..}
  jsonResponse dummy


------------------------------------------------------------------------------
handleCreateUser :: Handler App (AuthManager App) ()
handleCreateUser = do
  HTTPCreateUser{..} <- bodyJson
  let dummy = HTTPNewUser{id = Just 0, name = fullname, authenticated = False, ..}
  jsonResponse dummy


------------------------------------------------------------------------------
handleCreateDebate :: Handler App (AuthManager App) ()
handleCreateDebate = do
  HTTPCreateDebate{..} <- bodyJson
  let dummy = HTTPNewDebate{id = Just 0, ..}
  jsonResponse dummy

------------------------------------------------------------------------------
handleSubjects :: Handler App (AuthManager App) ()
handleSubjects = method GET allSubjects
    where
        allSubjects = jsonResponse $ [ SQLDebate{uid = 0, name = "dummy", description = "a topic", author = 0} ]

handleSubject :: Handler App (AuthManager App) ()
handleSubject = pathParam "debate" >>= \uid -> method GET (getSubject uid)
    where
        getSubject uid = jsonResponse $ SQLDebate{name = "dummy2", description = "another topic", author = 0, ..}

------------------------------------------------------------------------------
handleOpinions :: Handler App (AuthManager App) ()
handleOpinions = method GET allOpinions
    where
        allOpinions = jsonResponse $ [ Opinion{debate = 0, uid = 0, description = "Thing is bad", author = 0} ]

------------------------------------------------------------------------------
handleVotes :: Handler App (AuthManager App) ()
handleVotes = pathParam "debate" >>= \uid -> method POST (allVotes uid)
    where
        allVotes uid = jsonResponse $ Vote{voter = 0, debate = 0, option1 = 0, option2 = 1, ..}

handleVote :: Handler App (AuthManager App) ()
handleVote = do
    sid :: Integer <- pathParam "debate"
    vid :: Integer <- pathParam "vote"
    method POST pass

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = fmap (with auth) <$>
         [ ("attempt-login",                handleAttemptLogin)
         , ("logout",                       handleLogout)
         , ("create-user",                  handleCreateUser)
         , ("create-debate",                handleCreateDebate)

         , ("debate",                      handleSubjects)
         , ("debate/:debate",             handleSubject)
         , ("debate/:debate/opinion",     handleOpinions)
         , ("debate/:debate/vote",        handleVotes)
         , ("debate/:debate/vote/:vote",  handleVote)
         ]


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
