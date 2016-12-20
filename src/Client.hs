{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Client 
  ( runG1
  , runG2
  ) where 

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client
import Lib

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
    } deriving Generic
instance ToJSON RepoCommit

-- Response Data
data RepoInfoResponse = RepoInfoResponse
    {
        success     :: Bool
    } deriving (Show, Generic)
instance FromJSON RepoInfoResponse
instance ToJSON RepoInfoResponse

-- type
type NDSAPI = "group1" :> ReqBody '[JSON] RepoDataForProcessing :> Post '[JSON] RepoInfoResponse
      :<|> "group2" :> ReqBody '[JSON] RepoCommit :> Post '[JSON] RepoInfoResponse

group1 :: RepoDataForProcessing -- ^ value for the request body
          -> ClientM RepoInfoResponse

group2 :: RepoCommit -- ^ value for the request body
          -> ClientM RepoInfoResponse

ndsApi :: Proxy NDSAPI
ndsApi = Proxy

group1 :<|> group2 = client ndsApi

g1Queries :: ClientM (RepoInfoResponse)
g1Queries = do
  --g1 <- group1 (RepoDataForProcessing "www.RepoDataForProcessing_g1.git" ["Java", "Haskell"])
  g1 <- group1 repoDataForG1
  return (g1)

runG1 :: IO ()
runG1 = do
  manager <- newManager defaultManagerSettings
  res <- runClientM g1Queries (ClientEnv manager (BaseUrl Http "localhost" 8081 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (g1) -> do
      putStrLn $ "Group 1 Response: " ++ show g1 ++ " #End of Group 1 Response#"

g2Queries :: ClientM (RepoInfoResponse)
g2Queries = do
  --g2 <- group2 (RepoCommit "www.RepoCommit_g2.git" 1993 "Mingda")
  g2 <- group2 repoCommitForG2
  return (g2)

runG2 :: IO ()
runG2 = do
  manager <- newManager defaultManagerSettings
  res <- runClientM g2Queries (ClientEnv manager (BaseUrl Http "localhost" 8081 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (g2) -> do
       putStrLn $ "Group 2 Response: " ++ show g2 ++ " #End of Group 2 Response#"

-- Sample Data
repoDataForG1 :: RepoDataForProcessing
repoDataForG1 = RepoDataForProcessing "www.RepoDataForProcessing_g1.git" ["Java", "Haskell"]

repoCommitForG2 :: RepoCommit
repoCommitForG2 = RepoCommit "www.RepoCommit_g2.git" 1993 "Mingda"

