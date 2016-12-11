{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Lib
 (startApp)
 where

import           Prelude ()
import           Prelude.Compat

import           Data.Aeson.Types
import           Data.List
import           Data.String -- fromString
import           Data.ByteString.Char8 (pack)
import           Data.Maybe
import qualified Data.Text as T
import           GHC.Generics

import           Servant
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger

import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)

import           System.Environment           (getArgs, getProgName, lookupEnv)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)

import qualified GitHub as GH
import qualified GitHub.Endpoints.Users.Followers as Followers
import qualified GitHub.Endpoints.Users as Users

import qualified GitHub.Endpoints.Repos as Repos
import qualified GitHub.Endpoints.Repos.Commits as Github
import qualified GitHub.Data.Name as Name

import qualified GitHub.Auth as Auth

import qualified Data.HashMap.Strict as HM

-- loop for
import           Control.Monad.Cont
-- import           TotalCommit (totalCommit)

import Commit (showCommit)

type API = "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
      :<|> "post-repo-info" :> ReqBody '[JSON] [RepoInfo] :> Post '[JSON] RepoInfoResponse

newtype HelloMessage = HelloMessage { msg :: String }
  deriving Generic
instance ToJSON HelloMessage

-- Data Handler for Group 1
data RepoDataForProcessing = RepoDataForProcessing
    {
        git_url             :: String
      , language            :: [String]
    } deriving Generic
instance ToJSON RepoDataForProcessing

-- Data Handler for Group 2
data RepoCommit = RepoCommit
    {
         repo_url                :: String
       , number_of_commit        :: Int
       , last_commit             :: String
    } deriving (Generic, Show)
instance ToJSON RepoCommit

-- Data Handler for Group 3
data RepoInfo = RepoIno
    {   repo_name   :: String,
        owner       :: String,
        token       :: String
    } deriving Generic
instance ToJSON RepoInfo
instance FromJSON RepoInfo

data RepoInfoResponse = RepoInfoResponse
    {
        success     :: Bool
    } deriving Generic
instance ToJSON RepoInfoResponse

user_crawler = pack "vn09"
user_token = pack "3f16cd7951eb3b0a8d006c6a8286c3bb06dafcf7"
auth = (Just (Auth.BasicAuth user_crawler user_token))

-- startApp = showCommit
-- Start server here
startApp :: IO ()    -- set up wai logger for service to output apache style logging for rest calls
startApp = withLogging $ \ aplogger -> do
  warnLog "Starting use-haskell."

  forkIO $ taskScheduler 5

  let settings = setPort 8080 $ setLogger aplogger defaultSettings
  runSettings settings app

taskScheduler :: Int -> IO ()
taskScheduler delay = do
  warnLog $ "Task scheduler operating."

  threadDelay $ delay * 1000000
  taskScheduler delay -- tail recursion

-- | the function app calls serve, passing api and server. You can see that there is a kind of structure of function
-- composition here that is to do with how servant operates. The first parameter defines the type of the REST service,
-- (which is defined as type `Proxy API` - yes types of this formt are allowed - and we should note that the second part
-- of that type is out previously defined REST API) and the second parameter defines the implementation of the REST service.
app :: Application
app = serve api server

api :: Proxy API
api = Proxy

-- | And now we implement the REST service by matching the API type, providing a Handler method for each endpoint
-- defined in the API type. Normally we would implement each endpoint using a unique method definition, but this need
-- not be so. To add a news endpoint, define it in type API above, and add and implement a handler here.
server :: Server API
server = hello
     :<|> postRepoInfo

  where
        hello :: Maybe String -> Handler HelloMessage
        hello mname = return . HelloMessage $ case mname of
          Nothing -> "Hello, anonymous coward"
          Just n  -> "Hello, " ++ n

        -- Process info from group 3
        postRepoInfo :: [RepoInfo] -> Handler RepoInfoResponse
        postRepoInfo repos = liftIO $ do
            -- Variables to store data from group 1 and group 2
            let success = True

            forM_ repos $ \repo -> do
                let (h_owner, h_repo_name, h_token) = (g_repo_owner, g_repo_name, g_token)
                                            where g_repo_owner = owner repo
                                                  g_repo_name = repo_name repo
                                                  g_token     = token repo

                let group1_data = RepoCommit git_url number_of_commit last_commit
                        where git_url = "https://github.com/" ++ h_owner ++ "/" ++ h_repo_name ++ ".git"
                              number_of_commit = 1234
                              last_commit = liftIO $ do
                                possibleCommits <- Github.commit' auth (fromString h_owner) (fromString h_repo_name) "HEAD"
                                case possibleCommits of
                                    Left e  -> show $ "Error: " ++ (show e)
                                    Right c -> show $ formatCommit c

                print group1_data

                -- client group2_data
                --     where
                --         group2_data = RepoDataForProcessing git_url languages
                --         where git_url =
                --             languages = do
                --                 possibleLanguages <- Repos.languagesFor' auth (fromString h_owner) (fromString h_repo_name)
                --                 case possibleLanguages of
                --                         (Left error) -> "Error: " ++ (show e)
                --                         (Right languages) -> do
                --                             let listLanguagesKey = HM.keys languages
                --                             let listLanguages = map getLanguage listLanguagesKey
                --                             return listLanguages
            return $ RepoInfoResponse success

-- get lastCommit
-- getLastCommit :: String -> String -> Github.Commit
-- getLastCommit owner repo = do
--     -- possibleCommits <- Github.commit' auth (fromString h_owner) (fromString h_repo_name) "HEAD"
--     possibleCommits <- Github.commit' auth (fromString owner) (fromString repo) "HEAD"
--     case possibleCommits of
--         Left e  ->  show "aaa"
--         Right c ->  show $ formatCommit c

-- Get only commit thing
formatCommit :: Github.Commit -> String
formatCommit (Github.Commit (Name.N c) _ _ _ _ _ _ _) = T.unpack c

-- Get only language thing
getLanguage (Repos.Language name) = name
userGithubGetId :: Users.Name Users.User -> T.Text
userGithubGetId login = Users.untagName login

-- | error stuff
custom404Error msg = err404 { errBody = msg }

-- | Determines log reporting level. Set to "DEBUG", "WARNING" or "ERROR" as preferred. Loggin is
-- provided by the hslogger library.
logLevel :: IO String
logLevel = defEnv "LOG_LEVEL" id "DEBUG" True

-- | Logging stuff
iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

-- global loggin functions
debugLog, warnLog, errorLog :: String -> IO ()
debugLog = doLog debugM
warnLog  = doLog warningM
errorLog = doLog errorM
noticeLog = doLog noticeM

doLog f s = getProgName >>= \ p -> do
                t <- getCurrentTime
                f p $ (iso8601 t) ++ " " ++ s

withLogging act = withStdoutLogger $ \aplogger -> do

  lname  <- getProgName
  llevel <- logLevel
  updateGlobalLogger lname
                     (setLevel $ case llevel of
                                  "WARNING" -> WARNING
                                  "ERROR"   -> ERROR
                                  _         -> DEBUG)
  act aplogger


-- | Helper function to simplify the setting of environment variables
-- function that looks up environment variable and returns the result of running funtion fn over it
-- or if the environment variable does not exist, returns the value def. The function will optionally log a
-- warning based on Boolean tag
defEnv :: Show a
              => String        -- Environment Variable name
              -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
              -> a             -- default value to use if environment variable is not set
              -> Bool          -- True if we should warn if environment variable is not set
              -> IO a
defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing -> do
        when doWarn (doLog warningM $ "Environment variable: " ++ env ++
                                      " is not set. Defaulting to " ++ (show def))
        return def