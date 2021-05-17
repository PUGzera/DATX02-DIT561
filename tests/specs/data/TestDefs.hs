{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, StandaloneDeriving #-}
module TestDefs where

import Database.Daison
import Data.Data

-- Database: test.db
-- Use `select [snd x | x <- from TABLE everything]`
-- to examine how the front-end splits up data of a particular type
-- into columns.
-- Currently: only splits standalone tuples
--            i.e. tuples that are not part of another tuple, list or
--            datatype.

-- Defined tables:
-- either3       :: Table (Either (Either Int String) 
--                                (Either Float FilePath))
-- either3I      :: Table (Either (Either Int String) 
--                                (Either Float FilePath), Int)
-- empty         :: Table ()
-- emptyTuple    :: Table ()
-- emptyTuples   :: Table [()]
-- listOfNumbers :: Table [Int]
-- listOfTuples  :: Table [(Int,Int)]
-- maybeInt      :: Table (Maybe (Int,Int))
-- numbers       :: Table (FiftyTuple Int)
-- people        :: Table Person
-- people2       :: Table Person2
-- ssfb          :: Table (String, String, Float, Bool)
-- tupleInTuple  :: Table (Int, (Int, Float, Bool))


-- Check how the front-end displays nested data types, and ensure they are
-- parsed correctly (i.e. constructors before parentheses are still shown).
either3 = table "either3" :: Table (Either (Either Int String) (Either Float FilePath))

-- Confirm that columns after nested data types are displayed correctly
either3I = table "either3I" :: Table (Either (Either Int String) (Either Float FilePath), Int)

-- Empty database
empty = table "empty" :: Table ()

-- Ensure parentheses are parsed correctly, and that results from select
-- queries are printed directly to the terminal unformatted (as it only
-- contains a single empty tuple)
emptyTuple = table "emptyTuple" :: Table ()

-- Ensure parentheses are parsed correctly
emptyTuples = table "emptyTuples" :: Table [()]

-- Check how the front-end displays lists
listOfNumbers = table "listOfNumbers" :: Table [Int]

-- Ensure the different commas are interpreted correctly
listOfTuples = table "listOfTuples" :: Table [(Int,Int)]

-- Ensure that additional columns aren't created when a tuple is preceded by
-- a constructor
maybeInt = table "maybeInt" :: Table (Maybe (Int,Int))

-- Check how the front-end displays large tables that exceed the
-- terminal window's width and height
type FiftyTuple a = (a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a, a)
deriving instance Data a => Data (FiftyTuple a)
deriving instance Show a => Show (FiftyTuple a)
numbers = table "numbers" :: Table (FiftyTuple Int)

-- Check how the front-end displays custom data types
data Person = Person {name :: String, age :: Int} deriving Data
people = table "people" `withIndex` peopleName
peopleName = index people "name" name

-- Check how the front-end displays custom data types that have multiple 
-- constructors with different arguments
data Person2 = Student {name2 :: String, age2 :: Int}
             | Staff {name2 :: String, age2 :: Int, subjects :: [String]}
             deriving Data
people2 = table "people2" `withIndex` peopleName2
peopleName2 = index people2 "name" name2

-- Ensure standalone tuples are split up into columns properly,
-- as well as checking that characters such as `\n` and `\t` do not affect
-- formatting.
ssfb = table "ssfb" :: Table (String, String, Float, Bool)

-- Ensure tuples within tuples are not split into columns
tupleInTuple = table "tupleInTuple" :: Table (Int, (Int, Float, Bool))