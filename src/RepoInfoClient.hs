{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module RepoInfoClient where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant
import Servant.Client
import System.IO.Unsafe
import CommonLib as CL

type RepoInfoAPI = "hello" :> QueryParam "name" String :> Get '[JSON] CL.HelloMessage
      :<|> "post-repo-info" :> ReqBody '[JSON] [RepoInfo] :> Post '[JSON] CL.RepoInfoResponse

data RepoInfoResponse = RepoInfoResponse
    {
        success     :: Bool
    } deriving (Generic)
instance ToJSON RepoInfoResponse

hackageAPI :: Proxy RepoInfoAPI
hackageAPI = Proxy

hello        :: Maybe String -> ClientM CL.HelloMessage
postRepoInfo :: [CL.RepoInfo] -> ClientM RepoInfoResponse

hello :<|> postRepoInfo = client api