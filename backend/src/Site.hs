{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

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
import           Snap.Util.FileServe
import           Text.Read (readMaybe)
import qualified Heist.Interpreted as I
------------------------------------------------------------------------------
import           Application
import           Data

------------------------------------------------------------------------------

decodeParam :: (MonadSnap m, Read a) => ByteString -> m a
decodeParam name = do
    val <- getParam name
    case unpackChars <$> val >>= readMaybe of
        Just a -> return a
        Nothing -> do
            resp <- getResponse
            finishWith $
                setResponseStatus 400 ("Bad/missing path parameter ") $
                resp

jsonResponse :: (MonadSnap m, ToJSON a) => a -> m ()
jsonResponse a = do
    writeLBS $ encode a
    modifyResponse $ setHeader "Content-Type" "application/json"



------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe mempty splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

------------------------------------------------------------------------------
handleSubjects :: Handler App (AuthManager App) ()
handleSubjects = method GET allSubjects
    where
        allSubjects = jsonResponse $ [ Subject 0 "dummy" "a topic" ]

handleSubject :: Handler App (AuthManager App) ()
handleSubject = decodeParam "subject" >>= \sid -> method GET (getSubject sid)
    where
        getSubject sid = jsonResponse $ Subject sid "dummy2" "another topic"

------------------------------------------------------------------------------

handleOpinions :: Handler App (AuthManager App) ()
handleOpinions = method GET allOpinions
    where
        allOpinions = jsonResponse $ [ Opinion 0 "is bad" 1 ]

------------------------------------------------------------------------------
handleVotes :: Handler App (AuthManager App) ()
handleVotes = decodeParam "subject" >>= \sid ->
    method POST (allVotes sid)
    where
        opinion1 = Opinion 0 "All weapons should be banned" 0
        opinion2 = Opinion 1 "Three guns per child" 1
        allVotes sid = jsonResponse $ Vote 0 [opinion1, opinion2] sid

handleVote :: Handler App (AuthManager App) ()
handleVote = do
    sid :: Integer <- decodeParam "subject"
    vid :: Integer <- decodeParam "vote"
    method POST pass

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = fmap (with auth) <$>
         [ ("login",                        handleLoginSubmit)
         , ("logout",                       handleLogout)
         , ("new_user",                     handleNewUser)
         , ("hello",                        render "hello")

         , ("subject",                      handleSubjects)
         , ("subject/:subject",             handleSubject)
         , ("subject/:subject/opinion",     handleOpinions)
         , ("subject/:subject/vote",        handleVotes)
         , ("subject/:subject/vote/:vote",  handleVote)
         ] ++
         [ ("",         serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a
