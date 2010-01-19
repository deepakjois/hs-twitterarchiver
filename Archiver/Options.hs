module Archiver.Options where

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit


data Settings = TS {
  twitterUsername :: String,
  twitterPassword :: Maybe String,
  fileName :: String,
  sinceId  :: Maybe Integer } deriving Show

defaultSettings = TS { 
   twitterUsername = "vyom",
   twitterPassword = Nothing,
   fileName = "vyom.json",
   sinceId = Nothing }

options = [ Option "h" ["help"]
             (NoArg
                (\_ -> do prg <- getProgName
                          hPutStrLn stderr (usageInfo prg options)
                          exitWith ExitSuccess))
           "Show help"
         , Option "u" ["username"]
             (ReqArg
                (\arg opt -> return opt { twitterUsername = arg })
                "vyom")
           "Twitter Username"
         , Option "f" ["filename"]
             (ReqArg
                (\arg opt -> return opt { fileName = arg })
                "vyom.json")
           "Filename"
         , Option "p" ["password"]
             (NoArg
                (\opt -> do putStr "Enter Twitter Password : "
                            hFlush stdout
                            hSetEcho stdout False                            
                            password <- hGetLine stdin
                            hSetEcho stdout True
                            putStr "\n"
                            return opt { twitterPassword = Just password }))
           "ask for password (private Twitter stream)"
         ]

parseOptions = do args <- getArgs
                  -- Parse options, getting a list of option actions
                  let (actions, nonOptions, errors) = getOpt RequireOrder options args

                  -- Here we thread startOptions through all supplied option actions
                  foldl (>>=) (return defaultSettings) actions

