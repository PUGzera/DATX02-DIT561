module Main (
  main
) where

import qualified Frontend.GHCInterface as GHC
import Frontend.Run (run)

import System.Console.Haskeline
import System.Directory

folderName = "Daison-Frontend"
logName = ".daison_history"

main = do
    dataPath <- getAppUserDataDirectory folderName
    createDirectoryIfMissing True dataPath
    let fullPath = Just $ dataPath ++ '/' : logName
    let settings = defaultSettings{historyFile = fullPath}
    let input logInput = runInputT settings{autoAddHistory = logInput} . getInputLine
    
    run fullPath input