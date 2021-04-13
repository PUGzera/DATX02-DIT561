-- | Format Daison output as a table.
module Frontend.Format (
  formatTable
) where

import Frontend.Base
import Frontend.Typecheck
import qualified Frontend.GHCInterface as GHC

import Control.Monad ((<=<))
import Data.Char (isSpace)
import Data.List

-- | State record used in toRows
data ToRowsArgs = TRA {
    sqBrD :: Int, -- Square bracket depth
    parD :: Int, -- Parenthesis depth
    dQ :: Bool, -- Character is inside double quotes
    sQ :: Bool, -- Character is inside single quotes
    cStart :: Bool, -- Cell start
    cTuple :: Bool, -- Cell contents started with a parenthesis
    fromI :: Int, -- Cell start index
    toI :: Int, -- Cell end index
    rows :: [[String]], -- List of rows
    row :: [String], -- Row of cells
    str :: String, -- String to be formatted (consumed during execution)
    str' :: String -- Copy of str, used to extract substrings
}

-- | Format output from Daison queries to be more readable.
--   Assumes that a GHC session has been initialized.
formatTable :: String -> String -> DaisonI GHC.Doc
formatTable result expr 
    | length rows == 1 = singleRow
    | otherwise        = toTable expr rows
    where
        rows = postProcess . toRows $ resetFormat result
        row = head rows
        singleRow = do
            typeSig <- exprType <=< mToDaison $ expr
            return $ maybeBrackets ('[' == (head . (!! 1) . words) typeSig)
                     . GHC.maybeParens (multipleColumns row)
                     . GHC.hcat 
                     . GHC.punctuate (GHC.text ", ")
                     $ map GHC.text row

        maybeBrackets True = GHC.brackets
        maybeBrackets False = id
        multipleColumns row = length row > 1
        
        -- Remove the I64# constructor added to cells with 'Key a' values
        postProcess (r:rs) = pP' r : postProcess rs
        postProcess rs = rs
        pP' [] = []
        pP' (cell:r')
            | "I64# " `isPrefixOf` cell = drop 5 cell : pP' r'
            | otherwise                 = cell : pP' r'

-- | Separate a string into (at most) a two-dimensional list based on commas,
--   taking into account brackets, parentheses and quotation marks.
toRows :: String -> [[String]]
toRows result = toRows' $ TRA 0 0 False False True False 0 0 [] [] result result
    where
        toRows' :: ToRowsArgs -> [[String]]
        -- Input end
        toRows' args@TRA{str=""}
            | (not . null) (rows args) = rows args
            | otherwise                = [currentRow args]
        -- Result start (list)
        toRows' args@TRA{str='[':st, parD=0, sqBrD=0, dQ=False, sQ=False} 
            = toRows' (nextChar args){
                cStart = True,
                fromI = toI args + 1,
                sqBrD = sqBrD args + 1
            }
        -- Nested list start
        toRows' args@TRA{str='[':st, dQ=False, sQ=False} 
            = toRows' (nextChar args){
                sqBrD = sqBrD args + 1
            }
        -- Result end (list)
        toRows' args@TRA{str=']':st, parD=0, sqBrD=1, dQ=False, sQ=False} 
            = toRows' (nextChar args){
                sqBrD = sqBrD args - 1,
                rows = rows args ++ [currentRow args]
            }
        -- Nested list end
        toRows' args@TRA{str=']':st, dQ=False, sQ=False} 
            = toRows' (nextChar args){
                sqBrD = sqBrD args - 1
            }
        -- Result start (standalone tuple)
        toRows' args@TRA{str='(':st, cStart=True, parD=0, sqBrD=0, dQ=False, sQ=False} 
            = toRows' (nextChar args){
                cTuple = True,
                fromI = toI args + 1,
                parD = parD args + 1
            }
        -- Column value start (tuple in list)
        toRows' args@TRA{str='(':st, cStart=True, parD=0, sqBrD=1, dQ=False, sQ=False} 
            = toRows' (nextChar args){
                cTuple = True,
                fromI = toI args + 1,
                parD = parD args + 1
            }
        -- Nested tuple start
        toRows' args@TRA{str='(':st, dQ=False, sQ=False} 
            = toRows' (nextChar args){
                parD = parD args + 1
            }
        -- Result end (standalone tuple)
        toRows' args@TRA{str=')':st, cTuple=True, sqBrD=0, parD=1, dQ=False, sQ=False} 
            = toRows' (nextChar args){
                cTuple = False,
                parD = parD args - 1,
                row = row args ++ [substr' (fromI args) (toI args) (str' args)]
                }
        -- Column value end (tuple in list)
        toRows' args@TRA{str=')':st, cTuple=True, sqBrD=1, parD=1, dQ=False, sQ=False} 
            = toRows' (nextChar args){
                cTuple = False,
                parD = parD args - 1,
                row = row args ++ [substr' (fromI args) (toI args) (str' args)]
                }
        -- Nested tuple end
        toRows' args@TRA{str=')':st, dQ=False, sQ=False} 
            = toRows' (nextChar args){
                parD = parD args - 1
                }
        -- String start/end
        toRows' args@TRA{str='"':st, sQ=False}
            = toRows' (nextChar args){
                dQ = not $ dQ args
            }
        -- Char start/end
        toRows' args@TRA{str='\'':st, dQ=False} 
            = toRows' (nextChar args){
                sQ = not $ sQ args
            }
        -- Row separator
        toRows' args@TRA{str=',':st, cTuple=False, sqBrD=1, parD=0, dQ=False, sQ=False} 
            = toRows' (nextChar args){
                cStart = True,
                fromI = toI args + 1,
                rows = rows args ++ [currentRow args],
                row = []
            }
        -- Column separator (standalone tuple)
        toRows' args@TRA{str=',':st, cTuple=True, sqBrD=0, parD=1, dQ=False, sQ=False}
            = toRows' (nextChar args){
                cStart = True,
                fromI = toI args + 1,
                row = row args ++ [substr' (fromI args) (toI args) (str' args)]
            }
        -- Column separator (tuple in list)
        toRows' args@TRA{str=',':st, cTuple=True, sqBrD=1, parD=1, dQ=False, sQ=False}
            = toRows' (nextChar args){
                cStart = True,
                fromI = toI args + 1,
                row = row args ++ [substr' (fromI args) (toI args) (str' args)]
            }
        -- Leading whitespace
        toRows' args@TRA{str=' ':st, cStart=True}
            = toRows' (nextChar args){
                cStart = True,
                fromI = fromI args + 1
            }
        -- Characters that do not affect formatting
        toRows' args@TRA{str=_ch:st} 
            = toRows' $ nextChar args

        currentRow args = if (not . null) (row args) then row args
                          else [substr (fromI args) (toI args) (str' args)]

        nextChar args@TRA{str=_ch:st} = args{
            str = st, 
            toI = toI args + 1,
            cStart = False
            }

        substr' from to string
            | null substring = "()"
            | otherwise      = substring
            where substring = substr from to string

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