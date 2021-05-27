
module Frontend.Util (
    cd,
    printText,
    helpText,
    welcomeMsg,
    exitMsg
) where

import qualified Frontend.GHCInterface as GHC
import Frontend.Base

import System.Directory (doesDirectoryExist)

import Data.List.Split (splitOn)
import Data.List (intercalate)
import Data.Version (showVersion)
import Paths_daison_frontend (version)

winToUnix :: String -> String
winToUnix s = intercalate "/" (splitOn "\\" s)

unixToWin :: String -> String
unixToWin s = intercalate "\\" (splitOn "/" s)

cd' :: String -> String -> String
cd' ".." s = reverse $ dropWhile (/= '/') (reverse s)
cd' d s    = s ++ "/" ++ d

cd :: String -> DaisonI ()
cd s = do
    st <- getState
    let ud = cd' s $ winToUnix $ currentDirectory st
    id <- GHC.liftIO $ doesDirectoryExist ud
    if id then
        do modifyState (\st -> st { currentDirectory = ud } )
           GHC.liftIO $ putStrLn ("Working directory set to " ++ ud)
           return ()
        else do
            GHC.throw NoSuchDir
            return ()

-- | Prints the input on its own line in the console.
printText :: String -> DaisonI ()
printText =
    GHC.liftIO . putStrLn

helpText, welcomeMsg, exitMsg :: String
helpText =
    "Commands available from the prompt:\n" ++
    "   <statement>         Evaluate/run <statement>\n" ++
    "   :help, :?           Display this list of commands\n" ++
    "   :log path           Display the log file's path\n" ++
    "        show           Display the log file's contents\n" ++
    "        toggle         Enable/disable logging\n" ++
    "        wipe           Attempt to wipe the log file's contents\n" ++
    "   :type <expr>        Show the type of <expr>\n" ++
    "   :! <command>        Run the shell command <command>\n" ++
    "   :quit, :q           Quit the program\n" ++

    "\n\n" ++
    "   -- Commands for working with databases:\n" ++
    "\n" ++
    "   :dbs                Print the list of databases that are currently open\n" ++
    "   :open <name>        Open database with <name> or set focus to \n" ++
    "                       database with <name> if already open.\n" ++
    "                       This command creates a database with <name>\n" ++
    "                       if it doesn't exist.\n" ++
    "   :close <name>       Close database with <name> if opened\n" ++
    "   :mode [mode]        Set access mode (`ReadWrite` or `ReadOnly`)\n" ++
    "                       Displays the current access mode if no\n" ++
    "                       argument is given.\n" ++

    "\n\n" ++
    "   -- Commands for utility:\n" ++
    "\n" ++
    "   :cd <dir>           Set the current directory\n" ++
    "   :module <module>    Import <module>\n" ++
    "   :load <filepath>    Load a Haskell file from <filepath>\n" ++
    "   :set <option>       Set GHC <option>\n"

welcomeMsg = "Daison-Frontend, version " ++
                showVersion version ++
                "  :? for help\n\n" ++
             "Note: A log of user input is kept in order to enable arrow key navigation.\n" ++
             "      Use the help command for more information.\n"

exitMsg = "Leaving Daison-Frontend. Connections to open databases will be closed."
