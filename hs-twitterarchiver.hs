import Archiver.Options
import Archiver.Twitter

import Control.Monad.Reader

main = do settings <- parseOptions
          pastTweets <- runReaderT getPastTweets settings
          recentTweets <- runReaderT getRecentTweets (settings { sinceId = calculateSinceId pastTweets})
          let allTweets = recentTweets ++ pastTweets
              filename = fileName settings
          writeTweetsToFile filename allTweets
