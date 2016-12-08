{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Followers where
 
--import qualified GitHub.Endpoints.Users.Followers as GitHub
import qualified GitHub.Endpoints.Repos.Commits as Github
import GitHub.Data.Name 
import Data.List

-- import           Data.Aeson.Types
-- import           Data.List
import           Data.String -- fromString
import           Data.ByteString.Char8 (pack)
-- import           Data.Maybe
-- import qualified Data.Text as T

import           Prelude 
import           Prelude.Compat
-- import Prelude.Compat

-- import Data.Text         (Text, pack)
-- import Data.Text.IO as T (putStrLn)
-- import Data.Monoid       ((<>))

import qualified GitHub.Auth as Auth

-- showFollowers = do
--     possibleUsers <- GitHub.usersFollowing "vn09"
--     T.putStrLn $ either (("Error: " <>) . pack . show)
--                         (foldMap ((<> "\n") . formatUser))
--                         possibleUsers

-- formatUser :: GitHub.SimpleUser -> Text
-- formatUser = GitHub.untagName . GitHub.simpleUserLogin
user_crawler = pack "sneaker-rohit"
user_token = pack "57e41d5ff5155037ff7fd696fd6d1ae168b50e5c"
auth = (Just (Auth.BasicAuth user_crawler user_token))

showFollowers :: IO ()
showFollowers = do
	possibleCommits <- Github.commit' auth "sneaker-rohit" "github-crawler" "HEAD"
	print possibleCommits
 --    let a = True
 --    case a of 
	-- 	True -> putStrLn "true"
	-- 	False -> putStrLn ""
	case possibleCommits of 
 		Left e  -> putStrLn $ "Error: " ++ (show e)
		Right c -> putStrLn $ formatCommit c

formatCommit :: Github.Commit -> String
formatCommit (Github.Commit (Name c) _ _ _ _ _ _) = "commit " ++ (show c) 



-- 	case possibleCommits of
-- 	    (Left error)    -> putStrLn $ "Error: " ++ (show error)
-- 	    -- (Right commits) -> putStrLn $ intercalate "\n\n" $ formatCommit commits
-- 	    (Right commits) -> putStrLn $ getCommitSha commits

-- -- 	   -- (Right commits) -> putStrLn $ intercalate "\n\n" $ map formatCommit commits
-- formatCommit :: Github.Commit -> String
-- formatCommit commit = "commit" ++ (getCommitSha $ Github.commitSha commit)

-- getCommitSha(Github.mkName commit) = commit

-- getCommitSha :: Github.Name Github.Commit -> String
-- getCommitSha commitSha = "" ++ (Github.Commit commitSha)

-- getCommit :: Github.Name Github
-- --     "\nAuthor: " ++ (formatAuthor author) ++
-- --    "\nDate:   " ++ (show $ Github.fromDate $ Github.gitUserDate author) ++
--     "\n\n\t" ++ (Github.gitCommitMessage gitCommit)
--   where author = Github.gitCommitAuthor gitCommit
--         gitCommit = Github.commitGitCommit commit

-- formatAuthor :: Github.GitUser -> String
-- formatAuthor author =
--   (Github.gitUserName author) ++ " <" ++ (Github.gitUserEmail author) ++ ">"
