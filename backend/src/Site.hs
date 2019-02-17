{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, RecordWildCards, DuplicateRecordFields, ParallelListComp, NamedFieldPuns #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Fail
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (unpackChars)
import           Data.Aeson (encode, decode, ToJSON, FromJSON)
import           Data.Map.Syntax ((##))
import           Data.String (IsString (..))
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Util.FileServe
import           System.Random
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
  result <- loginByUsername username (ClearText $ fromString $ T.unpack password) True
  user <- case result of
    Left _ -> getResponse >>= finishWith . setResponseStatus 403 ("Failed auth")
    Right u -> return u
  jsonResponse user


------------------------------------------------------------------------------
handleLogout :: Handler App (AuthManager App) ()
handleLogout = do
  HTTPUser{..} <- bodyJson
  logout
  jsonResponse HTTPUser{authenticated = False, ..}


------------------------------------------------------------------------------
handleCreateUser :: Endpoint
handleCreateUser = do
  HTTPCreateUser{..} <- bodyJson
  createUser username (fromString $ T.unpack password)
  jsonResponse True


------------------------------------------------------------------------------
handleCreateDebate :: Endpoint
handleCreateDebate = do
  HTTPCreateDebate{..} <- bodyJson
  let dummy = HTTPNewDebate{id = Just 0, ..}
  jsonResponse dummy

------------------------------------------------------------------------------
handleDebateList :: Endpoint
handleDebateList = do
  let maxVal = 99999 :: Int
      count = [0..14] :: [Int]
  r1 <- traverse (\_ -> liftIO $ randomRIO (0, maxVal)) count
  r2 <- traverse (\_ -> liftIO $ randomRIO (0, maxVal)) count
  rb1 <- traverse (\_ -> liftIO $ randomIO) count
  rb2 <- traverse (\_ -> liftIO $ randomIO) count
  rb3 <- traverse (\_ -> liftIO $ randomIO) count
  let dummy = [ HTTPDebate
        { id = i
        , title = "Debate"
        , imageUrl = "https://i.huffpost.com/gadgets/slideshows/407618/slide_407618_5105750_free.jpg"
        , subtitle = "What to do"
        , description = "We're confused"
        , viewCount = v
        , opinionCount = o
        , bookmarked = b1
        , opined = b2
        , voted = b3 } | v <- r1 | o <- r2
                          | b1 <- rb1 | b2 <- rb2 | b3 <- rb3
                          | i <- [0..14]]
  jsonResponse dummy


------------------------------------------------------------------------------

bayesianRating :: Double -> -- |^ bayesian average
                  Int -> -- |^ bayesian vote count
                  Int -> -- |^ upvotes
                  Int -> -- |^ of total
                  Double -- |^ percentage rating in 0..1
bayesianRating avg count votesFor total =
  (fromIntegral count * avg + fromIntegral votesFor) /
  (fromIntegral count +       fromIntegral total)

getWins :: (HasPostgres m, MonadFail m) =>
           Id -> -- |^ opinion id
           m (Int, Int) -- |^ wins, total
getWins id = do
  -- TODO: this is slow, we should store the count in the db and increment
  [Only wins] <- query "SELECT COUNT(*) FROM votes WHERE winner=?" (Only id)
  [Only losses] <- query "SELECT COUNT(*) FROM votes WHERE loser=?" (Only id)
  return (wins, wins + losses)

getOpinionsWithRanking :: (HasPostgres m, MonadFail m) =>
                         Id -> -- |^ debate id
                         m [HTTPOpinion]
getOpinionsWithRanking id = do
  sqlOpinions <- query "SELECT * FROM opinions WHERE debate=?" (Only id)
  forM sqlOpinions $
    \SQLOpinion {uid, description, author, ..} -> do
      (wins, total) <- getWins uid
      let ranking = bayesianRating 0.5 10 wins total
      return $ HTTPOpinion
        { id = uid
        , authorId = author
        , description = description
        , ranking = ranking
        }

handleDebate :: Endpoint
handleDebate = do
  id <- pathParam "debate"
  viewCount <- liftIO $ randomRIO (0, 9999)
  [opined, voted, bookmarked] <- sequence $ replicate 3 (liftIO randomIO)

  let titles = ["What to have for lunch?", "Wall?", "Which song to play", "Java or JavaScript", "Which candidate?"]
  title <- (titles !!) <$> (liftIO $ randomRIO (0, length titles))

  let subtitle = ""
  let description = "I'm not sure"

  opinionCount <- liftIO $ randomRIO (10, 40)
  let list = replicate opinionCount ()
  opinions0 <- flip traverse list (\_ ->
    do win <- liftIO $ randomRIO (0, 15)
       lose <- liftIO $ randomRIO (0, 15)
       let ranking = bayesianRating 0.5 5 win (win + lose)
       id <- liftIO $ randomRIO (0, 999)
       authorId <- liftIO $ randomRIO (0, 999)
       return $ HTTPOpinion { description = "I like this" , ..  })
  let minRanking = foldr1 min (ranking <$> opinions0)
      maxRanking = foldr1 max (ranking <$> opinions0)
  let opinions = map (\a -> a { ranking = (ranking a - minRanking) / (maxRanking - minRanking) } ) opinions0

  let dummy = HTTPDebateDetail
        { title = "Debate"
        , imageUrl = "https://i.huffpost.com/gadgets/slideshows/407618/slide_407618_5105750_free.jpg"
        , subtitle = ""
        , description = description
        , myOpinion = Nothing
        , .. }
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
  did :: Id <- pathParam "debateId"
  HTTPNewOpinion {..} <- bodyJson
  -- [id] <- get from auth?
  let dummy = 0 :: Id
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
         , ("opinion-pair/:debateId",       post handleOpinionPair)
         , ("vote",                         post handleVote)
         , ("opine/:debateId",              post handleOpine)
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
    let dbEnabled = False
    if dbEnabled
      then do
        d <- nestSnaplet "db" db $ pgsInit' $ pgsDefaultConfig "host=localhost port=5432 dbname=debate"
        a <- nestSnaplet "auth" auth $ initPostgresAuth sess d
        addRoutes routes
        addAuthSplices h auth
        return $ App h s a d
      else do
        a <- nestSnaplet "auth" auth $ initJsonFileAuthManager defAuthSettings sess "users.json"
        addRoutes routes
        addAuthSplices h auth
        return $ App h s a undefined
