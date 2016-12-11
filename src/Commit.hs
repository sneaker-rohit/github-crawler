{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Commit
    (
        getLastCommit,
        getLanguage
    ) where


import qualified GitHub.Endpoints.Repos as Repos
import qualified GitHub.Endpoints.Repos.Commits as Commits
import qualified GitHub.Data.Name as Name
import qualified GitHub.Auth as Auth

import qualified Data.HashMap.Strict as HM

-- import Data.List as

import           Data.String (fromString) -- fromString
import           Data.ByteString.Char8 (pack)
import           Prelude
import           Prelude.Compat

import qualified Data.Text as T (strip, unpack, pack)
import           Text.HTML.TagSoup -- tagsoup

import           Network.HTTP.Client
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Status (statusCode)
import qualified Data.ByteString.Lazy.Char8 as B

import           Control.Monad.Cont (liftIO)

user_crawler = pack "scss-cs7051"
user_token = pack "677b3230c6ab76d126a54ed221ce0ee0765ce79c"

auth = (Just (Auth.BasicAuth user_crawler user_token))

------------------------------ Commit ------------------------------
-- Get last commit
getLastCommit :: String -> String -> IO String
getLastCommit owner repo_name = liftIO $ do
    possibleCommits <- Commits.commit' auth (fromString owner) (fromString repo_name) "HEAD"
    case possibleCommits of
        Left e  -> return $ "Error: " ++ (show e)
        Right c -> return $ show $ formatCommit c

formatCommit :: Commits.Commit -> String
formatCommit (Commits.Commit (Name.N c) _ _ _ _ _ _ _) = T.unpack c

convertStringToInt :: String -> Int
convertStringToInt str = read str :: Int
----------------------------------------------------------------------

------------------------------ Language ------------------------------
-- Get languages
getLanguage :: String -> String -> IO [String]
getLanguage owner repo = liftIO $ do
    possibleLanguages <- Repos.languagesFor' auth (fromString owner) (fromString repo)
    case possibleLanguages of
        (Left error) -> return []
        (Right languages) -> do
            let listLanguagesKey = HM.keys languages
            return $ map formatLanguage listLanguagesKey

formatLanguage (Repos.Language name) = T.unpack name
--------------------------------------------------------------------

