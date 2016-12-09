{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Commit
    (
        showCommit
    ) where

import qualified GitHub.Data as GH
import qualified GitHub.Data.Name (Name)
import qualified GitHub.Endpoints.Repos.Commits as Github
import qualified GitHub.Auth as Auth

import Data.List

import           Data.String -- fromString
import           Data.ByteString.Char8 (pack)
import           Prelude
import           Prelude.Compat

import qualified GitHub.Data.URL as GURL

user_crawler = pack "vn09"
user_token = pack "8bf7c20201a1cf30979158a2c7f2a160b43f1fb7"

auth = (Just (Auth.BasicAuth user_crawler user_token))

showCommit :: IO ()
showCommit = do
    possibleCommits <- Github.commit' auth "torvalds" "linux" "HEAD"
    print possibleCommits
    case possibleCommits of
        Left e  -> putStrLn $ "Error: " ++ (show e)
        Right c -> putStrLn $ formatCommit c

formatCommit :: Github.Commit -> String
formatCommit commit = show $ Github.commitSha commit

-- formatCommit (Github.Commit (GH.Name c) _ _ _ _ _ _) = "commit " ++ (show c)

