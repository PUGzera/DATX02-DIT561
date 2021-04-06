{-# LANGUAGE DeriveDataTypeable, MonadComprehensions #-}
-- | Functional tests, such as reading/writing to a database.
module Main (main) where
    
import Frontend.Run

import Database.Daison
import Test.QuickCheck
import Test.QuickCheck.Test

import Control.Monad
import Data.Data
import Data.IORef
import System.IO (stdout, openTempFile)
import GHC.IO.Handle
import System.Directory
import System.Exit

--  Test data types

data People = People {name :: String, age :: Int} deriving (Data, Show, Eq)

tPeople :: Table People
tPeople = table "People" `withIndex` iPeopleName

iPeopleName  :: Index People String
iPeopleName = index tPeople "Name" name


-- Functions for interacting with the front-end

-- | Send strings from an IORef to the front-end, one by one.
--   Empties the IORef.
getNextInput :: IORef [String] -> String -> IO (Maybe String)
getNextInput inputRef _prompt= do
    next <- readIORef inputRef
    case next of
        []       -> return Nothing
        cmd:cmds -> do
            modifyIORef' inputRef tail
            return $ Just cmd


-- Helper functions

-- | Create a temporary file given a test label and a file extension.
mkTempFileName :: String -> String -> String
mkTempFileName label ext
    | ext == ".db" = "test/" ++ base
    | otherwise    = base
    where
        base = "temp_" ++ label ++ ext

-- | Open a database given a test label.
openDB' :: String -> IO (FilePath, Database)
openDB' label = do
    let fileName = mkTempFileName label ".db"
    db <- openDB fileName
    return (fileName, db)

-- | Create a temporary text file in the test folder given a test label.
openTempFile' :: String -> IO (FilePath, Handle)
openTempFile' label = openTempFile "test" $ mkTempFileName label ".txt"

-- Test labels

readFromDatabase :: String
readFromDatabase = "readFromDatabase"


-- Test input commands

-- | Read People items from a pre-existing database.
input_readFromDatabase :: IO (IORef [String])
input_readFromDatabase = newIORef [
    "data People = People {name :: String, age :: Int} deriving (Data, Show, Eq)",
    "(tPeople, iPeopleName) = (table \"People\" `withIndex` iPeopleName :: Table People, index tPeople \"name\" name :: Index People String)",
    ":open " ++ mkTempFileName readFromDatabase ".db",
    "select [x | x <- from tPeople everything]",
    "it",
    ":q"
    ]

-- Tests

-- | Check if the front-end can read from a database.
prop_readFromDatabase :: [(String, Int)] -> Property
prop_readFromDatabase testData = ioProperty $ do
    -- Write and retrieve data using the back-end
    (dbFp, db) <- openDB' readFromDatabase
    ref <- runDaison db ReadWriteMode $ do
        tryDropTable tPeople
        tryCreateTable tPeople
        sequence_ $ map (\(s,i) -> insert tPeople (return (People s i))) testData
        select [x | x <- from tPeople everything]
    closeDB db

    -- Retrieve data using the front-end
    stdoutCopy <- hDuplicate stdout
    (fp, fh) <- openTempFile' readFromDatabase
    hDuplicateTo fh stdout
    cmds <- input_readFromDatabase
    run $ getNextInput cmds
    hDuplicateTo stdoutCopy stdout
    hClose fh

    output <- readFile fp
    let res = last . lines $ output

    -- Clean up
    removeFile fp
    removeFile dbFp

    return $ res == show ref

main :: IO ()
main = do
    result <- quickCheckWithResult (stdArgs {maxSuccess = 10}) prop_readFromDatabase
    unless (isSuccess result) exitFailure
