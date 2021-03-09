module Main (
  main
) where
import Frontend.Run
import System.Console.Haskeline

settings = defaultSettings {historyFile = Just "daison_history"}

main = run $ \str -> runInputT settings $ getInputLine str