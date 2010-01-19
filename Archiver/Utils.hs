module Archiver.Utils where

import Data.Maybe
import System.IO.Error

import Network.Browser
import Network.HTTP
import Network.URI

  
-- File related functions

readContentsArchiveFile f = do result <- try (readFile f)
                               case result of
                                 Right s -> do putStrLn "Reading archive file"
                                               return s

                                 _       -> do putStrLn "Could not read archive file"
                                               return ""

writeToFile filename tweetsString = do putStrLn "Writing to file"
                                       writeFile filename tweetsString

-- URL related functions

readContentsURLWithAuth url username password =   
  let nullHandler _ = return ()
  in do (_u, resp) <- browse $ do setOutHandler nullHandler
                                  checkAuth url username password
                                  (request $ getRequest url)
        case rspCode resp of
          (2,_,_) -> return (rspBody resp)
          _ -> fail ("Failed reading URL " ++ show url ++ " code: " ++ show (rspCode resp))

-- checks if password is provided and sets HTTP auth for request
checkAuth url username password 
   | password == Nothing = return ()
   | otherwise = do 
        ioAction $ putStrLn "Using HTTP Auth"
        addAuthority auth -- add auth to request
         where auth = AuthBasic {
                auUsername = username,
                auPassword = fromJust password,
                auRealm    = "",
                auSite      = fromJust $ parseAbsoluteURI url }
  