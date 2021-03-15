
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
    d <- GHC.liftIO getCurrentDirectory
    let ud = cd' s $ winToUnix d
    id <- GHC.liftIO $ doesDirectoryExist ud
    case id of
        True -> modifyState (\st -> st { currentDirectory = ud } )
        False -> return ()
