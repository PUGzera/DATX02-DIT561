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
    let fullPath = dataPath ++ '/' : logName
    let settings = defaultSettings {historyFile = Just fullPath}
    run $ runInputT settings . getInputLine 