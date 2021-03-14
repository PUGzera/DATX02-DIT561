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


-- Check if OS before calling winToUnix or unixToWin
cd :: String -> IO String
cd s = do
    d <- getCurrentDirectory
    let ud = cd' s $ winToUnix d
    id <- doesDirectoryExist ud
    return $ case id of
        True -> ud
        False -> winToUnix d
