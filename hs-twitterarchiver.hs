{-# LANGUAGE OverloadedStrings #-}
-- | Module providing the main hs-twitterarchiver program
--
-- Install the program by running
--
-- @
--     % cabal install hs-twitterarchiver
-- @
--
-- Then run the program with no arguments for help info
--
-- @
--      % hs-twitterarchiver
-- @
--
module Main (main) where

import Data.List (intercalate)
import Data.String (fromString)
import Control.Applicative ((<$>), (<*>), empty)
import System.IO.Error (try)
import Network.Browser (browse, setAllowRedirects, request)
import Network.HTTP (Response(..), getRequest, rspBody)
import System.Environment (getProgName, getArgs)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as B

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), object, decode, (.=), (.:))
import Data.Aeson.Encode.Pretty (encodePretty)

data Tweet = Tweet String  -- text
                   String  -- created at
                   Integer -- ID
                  deriving Show

tweetId :: Tweet -> Integer
tweetId (Tweet  _ _ i) = i

instance ToJSON Tweet where
  toJSON (Tweet t c i) = object [ "text"       .= t
                                , "created_at" .= c
                                , "id"         .= i
                                ]

instance FromJSON Tweet where
  parseJSON (Object v) = Tweet <$>
                        v .: "text" <*>
                        v .: "created_at" <*>
                        v .: "id"

  parseJSON _ = empty


-- Get array of Tweets from JSON String
readTweetsFromJSON :: B.ByteString -> [Tweet]
readTweetsFromJSON tweetsJSON = case decode tweetsJSON of
                                  Just t -> t
                                  _      -> []

-- Return the ID of the latest tweet in a list
sinceId :: [Tweet] -> Integer
sinceId []      = 0
sinceId tweets  = maximum $ map tweetId tweets

-- Return full URL for user's tweets
twitterUrl :: String -> [(String,String)] -> String
twitterUrl username params
  | params == [] = url
  | otherwise    = url ++ queryString
 where
  url = "http://api.twitter.com/1/" ++
        "statuses/user_timeline/" ++
        username ++ ".json"

  queryString = '?' : intercalate "&" (map (\(k,v) -> k ++ "=" ++ v) params)


-- Return list of tweets read from a file
readTweetsFromFile :: FilePath -> IO [Tweet]
readTweetsFromFile f = do
  result <- try (BS.readFile f >>= \r -> return (B.pack (BS.unpack r)))
  case result of
    Right json -> do
      putStrLn "Reading archive file"
      return $ readTweetsFromJSON json

    Left ex    -> do
      putStrLn "Could not read archive file."
      print ex
      return []

-- Write tweets to a given file
writeTweetsToFile :: FilePath -> [Tweet] -> IO ()
writeTweetsToFile file tweets = B.writeFile file $ encodePretty tweets

-- Fetch all newer tweets and return a list of all tweets
fetchTweets :: String -> Integer -> IO [Tweet]
fetchTweets username sinceId_ = fetchTweets' [] 1
 where
  additionalParams
      | sinceId_ == 0 = []
      | otherwise     = [("since_id", show sinceId_)]

  fetchTweets' tweetsSoFar page = do
    let params                   = [("count", "200"), ("page", show page)]
        url                      = twitterUrl username (params ++ additionalParams)
    putStrLn $ "Fetching tweets from " ++ url
    tweets   <- (readTweetsFromJSON . fromString) <$> fetchUrlResponse url
    putStrLn $ "Fetched " ++ show (length tweets) ++ " tweets"
    case tweets of
      [] -> return tweetsSoFar -- Return all tweets found so far
      _  -> fetchTweets' (tweetsSoFar ++ tweets) (page + (1 :: Integer)) -- Fetch next page

-- Fetch string response for given URL
fetchUrlResponse :: String -> IO String
fetchUrlResponse url = do
  (_, resp) <- browse $ do
    setAllowRedirects True
    request $ getRequest url
  case resp of
    result@(Response (2,_,_) _ _ _) -> return $ rspBody result
    Response code _ _ _             -> error $ "Unknown Response " ++ show code

-- Show usage information.
help :: IO ()
help = do
  name <- getProgName
  mapM_ putStrLn
    [ "ABOUT"
    , ""
    , "This is a Twitter stream archiver program."
    , ""
    , "It will try to read a JSON file in the current"
    , "folder and then fetch all newer tweets from Twtter"
    , "and store them in the same file."
    , ""
    , "For example usage, check https://github.com/deepakjois/TwitterArchive"
    , ""
    , "USAGE"
    , ""
    , name ++ " <username>          Fetch and store tweets for given handle <username>"
    , ""
    ]

-- Archive tweets of a user
archive :: String -> IO ()
archive username = do
  oldTweets <- readTweetsFromFile file
  newTweets <- fetchTweets username (sinceId oldTweets)
  let allTweets = newTweets ++ oldTweets
  putStrLn "Writing to archive file"
  writeTweetsToFile file allTweets
 where
  file = username ++ ".json"

-- | Main
main :: IO ()
main = do
  args <- getArgs
  case args of
    [username] -> archive username
    _          -> help
