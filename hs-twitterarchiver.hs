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

import Data.Either (rights)
import Data.List (intercalate)
import Control.Applicative ((<$>))
import System.IO.Error (try)
import Network.HTTP (Response(..), simpleHTTP, getRequest, rspBody)
import System.Environment (getProgName, getArgs)

import Text.JSON (JSON, Result(..), readJSON, showJSON, makeObj, resultToEither)
import Text.JSON.Types (JSValue(..), JSObject, fromJSObject)
import Text.JSON.String (runGetJSON, readJSArray)
import Text.JSON.Pretty (pp_value)

import Text.PrettyPrint (render)

data Tweet = Tweet String  -- text
                   String  -- created at
                   Integer -- ID,

tweetId :: Tweet -> Integer
tweetId (Tweet  _ _ i) = i

-- Making Tweet typeclass of JSON to enable decode/encode
instance JSON Tweet where
  showJSON (Tweet t c i) =
     makeObj [ ("text", showJSON t)
             , ("created_at", showJSON c)
             , ("id", showJSON i)
             ]

  readJSON (JSObject obj) = do
    i <- lookupM "id"
    t <- lookupM "text"
    c <- lookupM "created_at"
    return $ Tweet t c i
   where
     jsonObjAssoc = fromJSObject obj
     lookupM k     = maybe (Error "Property not found") readJSON $ lookup k jsonObjAssoc

  readJSON _ = undefined

-- Get array of Tweets from JSON String
readTweetsFromJSON :: String -> [Tweet]
readTweetsFromJSON tweetsJSON =
  case runGetJSON readJSArray tweetsJSON of
    Right (JSArray xs) -> rights $ map (resultToEither . readJSON) xs
    _                  -> []

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
  url = "http://twitter.com/" ++
        "statuses/user_timeline/" ++
        username ++ ".json"

  queryString = "?" ++ (intercalate "&" $ map (\(k,v) -> k ++ "=" ++ v) params)


-- Return list of tweets read from a file
readTweetsFromFile :: FilePath -> IO [Tweet]
readTweetsFromFile f = do
  result <- try (readFile f)
  case result of
    Right json -> do
      putStrLn "Reading archive file"
      return $ readTweetsFromJSON json

    Left ex    -> do
      putStrLn "Could not read archive file."
      putStrLn (show ex)
      return []

-- Write tweets to a given file
writeTweetsToFile :: FilePath -> [Tweet] -> IO ()
writeTweetsToFile file tweets = writeFile file $ (render . pp_value . showJSON) tweets

-- Fetch all newer tweets and return a list of all tweets
fetchTweets :: String -> [Tweet] -> IO [Tweet]
fetchTweets username oldTweets = fetchTweets' oldTweets 1
 where
  additionalParams
      | sinceId oldTweets == 0 = []
      | otherwise              = [("since_id", show $ sinceId oldTweets)]

  fetchTweets' tweetsSoFar page = do
    let params                   = [("count", "200"), ("page", show page)]
        url                      = twitterUrl username (params ++ additionalParams)
    putStrLn $ "Fetching tweets from " ++ url
    tweets   <- readTweetsFromJSON <$> fetchUrlResponse url
    case tweets of
      [] -> return tweetsSoFar -- Return all tweets found so far
      _  -> fetchTweets' (tweetsSoFar ++ tweets) (page + (1 :: Integer)) -- Fetch next page

-- Fetch string response for given URL
fetchUrlResponse :: String -> IO String
fetchUrlResponse url = do
  resp <- simpleHTTP (getRequest url)
  case resp of
    Left err                              -> error (show err)
    Right result@(Response (2,_,_) _ _ _) -> return $ rspBody result
    Right (Response code _ _ _)           -> error $ "Unknown Response " ++ show code

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
  oldTweets <- readTweetsFromFile $ file
  allTweets <- fetchTweets username oldTweets
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
