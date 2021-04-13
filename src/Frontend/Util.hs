
module Frontend.Util (
    cd,
    printText,
    helpText,
    welcomeMsg,
    exitMsg
) where

import Frontend.Base

import qualified Frontend.GHCInterface as GHC

import System.Environment

import System.Directory

import Data.List.Split

import Data.List


import Paths_daison_frontend (version)
import Data.Version (showVersion)

winToUnix :: String -> String
winToUnix s = intercalate "/" (splitOn "\\" s)

unixToWin :: String -> String
unixToWin s = intercalate "\\" (splitOn "/" s)

cd' :: String -> String -> String
cd' ".." s = reverse $ dropWhile (\c -> c /= '/') (reverse s)
cd' d s    = s ++ "/" ++ d


-- ToDo: Check if OS before calling winToUnix or unixToWin
cd :: String -> DaisonI ()
cd s = do
    st <- getState
    let ud = cd' s $ winToUnix $ currentDirectory st
    id <- GHC.liftIO $ doesDirectoryExist ud
    case id of
        True -> do
            modifyState (\st -> st { currentDirectory = ud } )
            GHC.liftIO $ print ud
            return ()
        False -> return ()

-- | Prints the input on its own line in the console.
printText :: String -> DaisonI ()
printText = 
    GHC.liftIO . putStrLn 

helpText,welcomeMsg,exitMsg :: String
helpText = 
    "Commands available from the prompt:\n" ++
    "   <statement>         evaluate/run <statement>\n" ++
    "   :dbs                print the list of databases that are currently open\n" ++
    "   :help, :?           display this list of commands\n" ++
    "   :t <expr>           show the type of <expr>\n" ++
    "   :q, :quit           quit the program\n" ++

    "\n" ++
    "-- Commands for working with databases:\n" ++
    "   :close <name>       close database with <name> if opened\n" ++
    "   :db, :open <name>   open database with <name> or set focus to \n" ++
    "                       database with <name> if already open\n" ++

    "\n" ++
    "-- Commands for utility:\n" ++
    "   :cd <dir>           set the current directory\n" ++
    "   :m <module>         import <module>\n" ++
    "   :l <filepath>       load a haskell file from <filepath>\n" ++
    "   :set <option>       set <option>\n"

welcomeMsg = "Daison-Frontend, version " ++ 
                showVersion version ++
                "  :? for help"

exitMsg = "Leaving Daison-Frontend. Connections to open databases will be closed."
