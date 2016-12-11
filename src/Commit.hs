{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Commit
    (
        showCommit
    ) where

import qualified GitHub.Data.Name as Name

import qualified GitHub.Endpoints.Repos.Commits as Github
import qualified GitHub.Auth as Auth

import Data.List

import           Data.String -- fromString
import           Data.ByteString.Char8 (pack)
import           Prelude
import           Prelude.Compat

import qualified Data.Text as T

user_crawler = pack "vn09"
user_token = pack "3f16cd7951eb3b0a8d006c6a8286c3bb06dafcf7"

auth = (Just (Auth.BasicAuth user_crawler user_token))

showCommit :: IO ()
showCommit = do
    possibleCommits <- Github.commit' auth "torvalds" "linux" "HEAD"
    case possibleCommits of
        Left e  -> putStrLn $ "Error: " ++ (show e)
        Right c -> putStrLn $ show $ formatCommit c

formatCommit :: Github.Commit -> String
formatCommit (Github.Commit (Name.N c) _ _ _ _ _ _ _) = T.unpack c
