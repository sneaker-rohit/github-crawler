module TotalCommit
    (
        getTotalCommit
    )
    where

import Text.HTML.TagSoup

import Network.HTTP.Client
import Network.HTTP.Client.TLS   (tlsManagerSettings)
import qualified Data.ByteString.Lazy.Char8 as B

import Data.Text (strip, unpack, pack)

-------------------------Total commit ------------------------------
openURL :: String -> IO String
openURL url = do
    manager <- newManager tlsManagerSettings

    request <- parseRequest url
    response <- httpLbs request manager

    return $ B.unpack $ responseBody response

-- Get to total commit by parsing <span class='num text-emphasized'> element
-- from GITHUB html page.

-- Int value from IO Int type can be parsed easily in IO by "<-" sign.
-- For example:
--   main :: IO ()
--   main = do
--      a <- totalCommit -- a will have type Int
getTotalCommit :: String -> IO Int
getTotalCommit url =  do
    tags <- parseTags <$> openURL url
    let commitNumber = fromTagText (dropWhile (~/= "<span class='num text-emphasized'>") tags !! 1)
    return $ convertStringToInt $ unpack $ strip $ pack commitNumber

convertStringToInt :: String -> Int
convertStringToInt str = read str :: Int
--------------------------------------------------------------------


