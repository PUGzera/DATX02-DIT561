module Main (
  main
) where
import Frontend.Run (run)
import System.Console.Haskeline

settings = defaultSettings {historyFile = Just "daison_history"}

main = run $ runInputT settings . getInputLine 