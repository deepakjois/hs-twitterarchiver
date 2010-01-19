module Archiver.Twitter where

import Archiver.Options
import Archiver.Utils

import Text.JSON
import Text.JSON.Types
import Text.JSON.String
import Text.JSON.Pretty

import Data.List

import Control.Applicative
import Control.Monad.Reader

data Tweet = Tweet { 
  tweetText      :: String, 
  tweetCreatedAt :: String , 
  tweetId        :: Integer } deriving Show

-- Making Tweet typeclass of JSON to enable decode/encode
instance JSON Tweet where
   showJSON (Tweet tweetText tweetCreatedAt tweetId) = makeObj [ ("text", showJSON tweetText),
                                                                 ("created_at", showJSON tweetCreatedAt), 
                                                                 ("id", showJSON tweetId)]

   readJSON (JSObject obj) = let jsonObjAssoc = fromJSObject obj
                                 mLookup a = maybe (fail $ "No such element: " ++ a) return . lookup a
                                 lookupP p = mLookup p jsonObjAssoc >>= readJSON
                             in do i <- lookupP "id"
                                   t <- lookupP "text"
                                   c <- lookupP "created_at"
                                   return $ Tweet t c i

twitterUrl  = "http://twitter.com/"

-- extract array of JSON values from a string
readJSONTweets tweetsJSONString = case runGetJSON readJSArray tweetsJSONString of
                                    Right (JSArray xs) -> xs
                                    _                  -> []


-- read twitter stream page by page
readTwitterStream page tweets
    | page >= 21 = return tweets
    | otherwise  = do fullUrl <- constructFullUrl
                      tweetsJSON <- readJSONTweets <$> readContentsURL fullUrl
                      if not (null tweetsJSON)
                        then readTwitterStream (page + 1) (tweets ++ tweetsJSON)
                        else return tweets
                   where url username          = twitterUrl ++ "statuses/user_timeline/" ++ username ++ ".json"

                         queryParams           = [("count", "200"), ("page", show page)]

                         concatQueryStr params = intercalate "&" $ map (\(k,v) -> k ++ "=" ++ v) params

                         querystring Nothing        =  concatQueryStr queryParams
                         querystring (Just tweetId) =  concatQueryStr $ queryParams ++ [("since_id", show tweetId)]

                         constructFullUrl  = do username <- asks twitterUsername
                                                sinceId  <- asks sinceId
                                                return $ url username ++ "?" ++ querystring sinceId

-- read contents of URL w/ optional HTTP auth
readContentsURL url = do
 liftIO $ putStrLn url
 username <- asks twitterUsername
 password <- asks twitterPassword 
 liftIO $ readContentsURLWithAuth url username password


 -- calculate latest id for since_id param
calculateSinceId pastTweets = if not (null pastTweets)
                               then
                                 let (Ok tweetids) = forM pastTweets $ liftM tweetId . readJSON
                                 in Just (maximum tweetids)
                               else Nothing

                   
-- get past tweets stored in file (if any)
getPastTweets :: ReaderT Settings IO [JSValue]
getPastTweets = do filename <- asks fileName
                   fileContents <- liftIO $ readContentsArchiveFile filename
                   return $ readJSONTweets fileContents


-- read twitter stream
getRecentTweets :: ReaderT Settings IO [JSValue]
getRecentTweets = readTwitterStream 1 [] 


-- write Tweets to file
writeTweetsToFile filename tweetsJSON = let (Ok tweets) = mapM readJSONTweet tweetsJSON
                                            readJSONTweet :: JSValue -> Result Tweet
                                            readJSONTweet = readJSON
                                            tweetsString    =  render $  pp_value  $ showJSON tweets -- Encoding to JSON
                                        in writeToFile filename tweetsString
