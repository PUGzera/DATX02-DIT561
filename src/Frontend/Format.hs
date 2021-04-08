-- | Format Daison output as a table.
module Frontend.Format (
  formatTable
) where

import Frontend.Base
import Frontend.Typecheck
import qualified Frontend.GHCInterface as GHC

import Data.Char (isSpace)
import Data.List

-- | State record used in toRows
data ToRowsArgs = TRA {
    sqBrD :: Int, -- Square bracket depth
    parD :: Int, -- Parenthesis depth
    dQ :: Bool, -- Character is inside double quotes
    sQ :: Bool, -- Character is inside single quotes
    raw :: Bool, -- Flag to handle backslashes in strings (i.e. when dQ=True)
    fromI :: Int, -- Cell start index
    toI :: Int, -- Cell end index
    rows :: [[String]], -- List of rows
    row :: [String], -- Row of cells
    str :: String, -- String to be formatted (consumed during execution)
    str' :: String -- Copy of str, used to extract substrings
}

-- | Format output from Daison queries to be more readable.
formatTable :: String -> String -> DaisonI GHC.Doc
formatTable result expr 
    | length rows == 1 = singleRow rows 
    | otherwise        = toTable expr . postProcess $ rows
    where
        rows = toRows $ resetFormat result
        singleRow row = do
            typeSig <- exprType expr
            return $ GHC.brackets 
                     . GHC.maybeParens (',' `elem` typeSig)
                     . GHC.hcat 
                     . GHC.punctuate (GHC.text ", ")
                     $ map GHC.text $ head rows
        
        -- Remove the I64# constructor added to cells with 'Key a' values
        postProcess ((cell:r'):rs) 
            | "I64# " `isPrefixOf` cell = (drop 5 cell : r') : postProcess rs
        postProcess rs = rs

-- | Separate a string into (at most) a two-dimensional list based on commas,
--   taking into account brackets, parentheses and quotation marks.
toRows :: String -> [[String]]
toRows result = toRows' $ TRA 0 0 False False False 0 0 [] [] result result
    where
        toRows' :: ToRowsArgs -> [[String]]
        -- Input end
        toRows' args@TRA{str=""}
            | (not . null) (rows args) = rows args
            | otherwise                = [currentRow args]
        -- Result start
        toRows' args@TRA{str='[':st, sqBrD=0, dQ=False, sQ=False} 
            = toRows' args{
                str = st,
                fromI = toI args + 1,
                toI = toI args + 1,
                sqBrD = sqBrD args + 1
            }
        -- Nested list start
        toRows' args@TRA{str='[':st, dQ=False, sQ=False} 
            = toRows' args{
                str = st,
                toI = toI args + 1,
                sqBrD = sqBrD args + 1
            }
        -- Result end
        toRows' args@TRA{str=']':st, sqBrD=1, dQ=False, sQ=False} 
            = toRows' args{
                str = st,
                sqBrD = sqBrD args - 1,
                rows = rows args ++ [currentRow args]
            }
        -- Nested list end
        toRows' args@TRA{str=']':st, dQ=False, sQ=False} 
            = toRows' args{
                str = st,
                toI = toI args + 1,
                sqBrD = sqBrD args - 1
            }
        -- Column value start
        toRows' args@TRA{str='(':st, parD=0, dQ=False, sQ=False} 
            = toRows' args{
                str = st,
                fromI = toI args + 1,
                toI = toI args + 1,
                parD = parD args + 1
            }
        -- Nested tuple start
        toRows' args@TRA{str='(':st, dQ=False, sQ=False} 
            = toRows' args{
                str = st,
                toI = toI args + 1,
                parD = parD args + 1
            }
        -- Column value end
        toRows' args@TRA{str=')':st, parD=1, dQ=False, sQ=False} 
            = toRows' args{
                str = st,
                toI = toI args + 1,
                parD = parD args - 1,
                row = row args ++ [substr (fromI args) (toI args) (str' args)]
                }
        -- Nested tuple end
        toRows' args@TRA{str=')':st, dQ=False, sQ=False} 
            = toRows' args{
                str = st,
                toI = toI args + 1,
                parD = parD args - 1
                }
        -- String start/end
        toRows' args@TRA{str='"':st, sQ=False, raw=False} 
            = toRows' args{
                str = st,
                toI = toI args + 1,
                dQ = not $ dQ args
            }
        -- Char start/end
        toRows' args@TRA{str='\'':st, dQ=False} 
            = toRows' args{
                str = st,
                toI = toI args + 1,
                sQ = not $ sQ args
            }
        -- Treat the next character as a raw string character
        toRows' args@TRA{str='\\':st, sQ=False, raw=False} 
            = toRows' args{
                str = st,
                toI = toI args + 1,
                raw = True
            }
        -- Row separator
        toRows' args@TRA{str=',':st, sqBrD=1, parD=0, dQ=False, sQ=False} 
            = toRows' args{
                str = st,
                fromI = toI args + 1,
                toI = toI args + 1,
                rows = rows args ++ [currentRow args],
                row = []
            }
        -- Column separator
        toRows' args@TRA{str=',':st, sqBrD=1, parD=1, dQ=False, sQ=False}
            = toRows' args{
                str = st,
                fromI = toI args + 1,
                toI = toI args + 1,
                row = row args ++ [substr (fromI args) (toI args) (str' args)]
            }
        -- Characters that do not affect formatting
        toRows' args@TRA{str=_ch:st} 
            = toRows' args{
                str = st,
                toI = toI args + 1,
                raw = False
            }

        currentRow args = if (not . null) (row args) then row args
                          else [substr (fromI args) (toI args) (str' args)]

-- | Format a two-dimensional list as a table.
--   Prepends information about the query used to generate it as well as the
--   type signature of the result.
--   Additionally, an extra column denoting each row's position in the result
--   is added to the table. 
toTable :: String -> [[String]] -> DaisonI GHC.Doc
toTable expr processedRows = do
    let queryText = GHC.text expr
    typeSig <- mToDaison expr >>= exprType >>= removeDaison

    let typeText = GHC.text $ "it :: " ++ typeSig
    let header = map (dropWhile isSpace) $ getLabels $ resetFormat typeSig

    let indexCol = "it !!" : map show [0..length processedRows - 1]
    let rowsT = indexCol : transpose (header : processedRows)
    let maxSizes = map (maximum . map length) rowsT
    let headerSep = intercalate "-+-" $ map (`replicate` '-') maxSizes

    let paddedRows = map (intercalate " | ") $ transpose . padColumns maxSizes $ rowsT
    let formattedRows = head paddedRows : headerSep : tail paddedRows

    let table = (GHC.vcat . map GHC.text) formattedRows
    return $ GHC.vcat [queryText, typeText, GHC.char ' ', table]


-- | Remove the Daison prefix from a type signature.
removeDaison :: String -> DaisonI String
removeDaison str
    | "Daison " `isPrefixOf` str = return $ drop 7 str
    | otherwise                  = return str

-- | Remove newlines and excess spacing from a string.
resetFormat :: String -> String
resetFormat = unwords . words

-- | Get the labels for the header row.
--   Key labels have their type arguments omitted.
getLabels :: String -> [String]
getLabels typeSig = postProcess $ head . toRows $ typeSig
    where
        postProcess (cell:row')
            | "Key" `isPrefixOf` cell = "Key" : postProcess row'
        postProcess row = row

-- | Ensure every cell in a column takes up the same amount of space.
padColumns :: [Int] -> [[String]] -> [[String]]
padColumns _ [] = []
padColumns (n:ns) (rowT:rowsT) = map addExtraSpaces rowT : padColumns ns rowsT
    where
        addExtraSpaces cell = cell ++ replicate (n - length cell) ' '

-- | Return a substring, where 'from' is inclusive and 'to' is exclusive.
substr :: Int -> Int -> String -> String
substr from to = drop from . take to