{-# LANGUAGE DeriveDataTypeable, MonadComprehensions #-}
-- | Functional tests, such as reading/writing to a database.
module Main (main) where
    
import Frontend.Run

import Database.Daison hiding (tableName, createTable)
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

data Person = Person {name :: String, age :: Int} deriving (Data, Show, Eq)
instance Arbitrary Person where
    arbitrary = applyArbitrary2 $ \s i -> Person s i

-- Test values
tableName :: String
tableName = "testTable"

testTable :: Data a => Table a -- needs type casting
testTable = table tableName

-- Functions for interacting with the front-end

-- | Send strings from an IORef to the front-end, one by one.
--   Empties the IORef.
getNextInput :: IORef [String] -> Bool -> String -> IO (Maybe String)
getNextInput inputRef _logInput _prompt = do
    next <- readIORef inputRef
    case next of
        []       -> return Nothing
        cmd:cmds -> do
            modifyIORef' inputRef tail
            return $ Just cmd


-- Helper functions

-- | Create a string that defines a table within the GHC session
--   e.g. testTable = table "testTable" :: Table Person
createTable :: String -> String
createTable tType = tableName ++ " = table \"" ++ tableName ++ "\" :: Table " ++ tType

-- | Create a temporary file given a test label and a file extension.
mkTempFileName :: String -> String -> String
mkTempFileName label ext
    | ext == ".db" = "tests/" ++ base
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
openTempFile' label = openTempFile "tests" $ mkTempFileName label ".txt"

-- | Run the front-end and simulate user input. Does not print to the
--   console. Returns a file name whose contents depends on input_test.
runSilent :: String -> IO (IORef [String])  -> IO FilePath
runSilent testLabel input_test = do
    stdoutCopy <- hDuplicate stdout
    (fp, fh) <- openTempFile' testLabel
    hDuplicateTo fh stdout
    cmds <- input_test
    run Nothing $ getNextInput cmds
    hDuplicateTo stdoutCopy stdout
    hClose fh
    return fp

-- Test labels

readFromDatabase :: String
readFromDatabase = "readFromDatabase"

writeToDatabase :: String
writeToDatabase = "writeToDatabase"

-- Test input commands

-- | Read items from a pre-existing database.
input_readFromDatabase ::  String -> [String] -> IO (IORef [String])
input_readFromDatabase tableName inputCode = newIORef $ 
    (":open " ++ mkTempFileName readFromDatabase ".db") : inputCode
    ++ [
    "select [x | (_key,x) <- from " ++ tableName ++ " everything]",
    "it",
    ":q"
    ]

-- | Write items to a new database.
--   Assumes that (show a) can be used to define a value of type a
input_writeToDatabase :: (Arbitrary a, Data a, Show a, Eq a) => String -> [String] -> [a] -> IO (IORef [String])
input_writeToDatabase tableName inputCode testData = newIORef $ 
    (":open " ++ mkTempFileName writeToDatabase ".db") : inputCode
    ++ [
    "createTable " ++ tableName,
    "mapM_ (insert_ " ++ tableName ++ ") " ++ show testData,
    ":q"
    ]

-- Tests

-- | Check if the front-end can read from a database.
prop_readFromDatabase :: (Arbitrary a, Data a, Show a, Eq a) => Table a -> [String] -> [a] -> Property
prop_readFromDatabase testTable' inputCode testData = ioProperty $ do
    -- Write and retrieve data using the back-end
    (dbFp, db) <- openDB' readFromDatabase
    ref <- runDaison db ReadWriteMode $ do
        tryDropTable testTable'
        tryCreateTable testTable'
        mapM_ (insert_ testTable') testData
        select [x | (_key,x) <- from testTable' everything]
    closeDB db

    -- Read data using the front-end
    fp <- runSilent readFromDatabase $ input_readFromDatabase tableName inputCode

    output <- readFile fp
    let outputLines = lines output
    let res = outputLines !! (length outputLines - 2)

    -- Clean up
    removeFile fp
    removeFile dbFp

    let success = res == show ref
    unless success $ do
        print $ "Expected: " ++ show ref
        print $ "Actual:   " ++ res
        
    return success

-- | Check if the front-end can read from a database.
prop_writeToDatabase :: (Arbitrary a, Data a, Show a, Eq a) => Table a -> [String] -> [a] -> Property
prop_writeToDatabase testTable' inputCode testData = ioProperty $ do
    -- Write data using the front-end
    fp <- runSilent writeToDatabase $ input_writeToDatabase tableName inputCode testData

    -- Read data using the back-end
    (dbFp, db) <- openDB' writeToDatabase
    res <- runDaison db ReadWriteMode $ select [x | (_key,x) <- from testTable' everything]
    closeDB db

    -- Clean up
    removeFile fp
    removeFile dbFp

    let success = res == testData
    unless success $ do
        print $ "Expected: " ++ show testData
        print $ "Actual:   " ++ show res
        
    return success

-- Arguments for properties
tPerson = testTable :: Table Person
tDoubless = testTable :: Table [Double]
tTuples = testTable :: Table (Bool, Int)
tChars = testTable :: Table Char

iCPerson = ["data Person = Person {name :: String, age :: Int} deriving (Data, Show, Eq)",
            createTable "Person"
           ]
iCDoubless = [createTable "[Double]"]
iCTuples = [createTable "(Bool,Int)"]
iCChars = [createTable "Char"]

main :: IO ()
main = do
    -- Read using the front-end
    print "Reading from database with table :: Table Person"
    res11 <- quickCheckWithResult stdArgs $ prop_readFromDatabase tPerson iCPerson
    print "Reading from database with table :: Table [Double]"
    res12 <- quickCheckWithResult stdArgs $ prop_readFromDatabase tDoubless iCDoubless
    print "Reading from database with table :: Table (Bool, Int)"
    res13 <- quickCheckWithResult stdArgs $ prop_readFromDatabase tTuples iCTuples
    print "Reading from database with table :: Table Char"
    res14 <- quickCheckWithResult stdArgs $ prop_readFromDatabase tChars iCChars

    -- Write using the front-end
    print "Writing to database with table :: Table Person"
    res21 <- quickCheckWithResult stdArgs $ prop_writeToDatabase tPerson iCPerson
    print "Writing to database with table :: Table [Double]"
    res22 <- quickCheckWithResult stdArgs $ prop_writeToDatabase tDoubless iCDoubless
    print "Writing to database with table :: Table (Bool, Int)"
    res23 <- quickCheckWithResult stdArgs $ prop_writeToDatabase tTuples iCTuples
    print "Writing to database with table :: Table Char"
    res24 <- quickCheckWithResult stdArgs $ prop_writeToDatabase tChars iCChars

    let tests = [res11, res12, res13, res14,
                 res21, res22, res23, res24]

    unless (all isSuccess tests) exitFailure
