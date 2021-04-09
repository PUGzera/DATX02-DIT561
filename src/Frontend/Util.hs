
module Frontend.Util (
    cd
) where

import Frontend.Base

import qualified Frontend.GHCInterface as GHC

import System.Environment

import System.Directory

import Data.List.Split

import Data.List

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

