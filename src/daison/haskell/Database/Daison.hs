{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, BangPatterns, CPP #-}
module Database.Daison
            ( Database, openDB, closeDB

            , runDaison, AccessMode(..), Daison

            , Key
            , Table, table, tableName
            , Index, index, listIndex, maybeIndex, withIndex
            , indexedTable, applyIndex
            , createTable, tryCreateTable
            , dropTable, tryDropTable
            , alterTable, renameTable
            , withForeignKey, withCascadeDelete, withCascadeUpdate
            
            , DataSet, dataset, datasetName
            , createDataSet, tryCreateDataSet
            , dropDataSet, tryDropDataSet
            , insertData, deleteData

            , Query, QueryMonad
            , select, query, anyOf

            , Aggregator
            , listRows, distinctRows
            , firstRow, lastRow, topRows, bottomRows
            , foldRows, foldRows1
            , groupRows, groupRowsWith, groupRowsBy, groupRowsByWith
            , having
            , sortRows,  sortRowsBy
            , sumRows, averageRows, countRows

            , At, at
            , Restriction, everything, asc, desc, (^>=), (^>), (^<=), (^<)
            , From(K,V), from, FromIndex(VI,VL), fromIndex, fromList

            , store
            , insert, insert_
            , update, update_
            , delete, delete_
            ) where

import Prelude hiding (last)
import Foreign
import Foreign.C
import Data.Data
import Data.IORef
import Data.ByteString(ByteString,append,null,empty)
import Data.ByteString.Unsafe(unsafeUseAsCStringLen,unsafePackCStringLen,unsafePackMallocCStringLen)
import Data.Maybe(maybeToList, fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List
import Control.Exception(Exception,throwIO,bracket,bracket_,onException)
import Control.Applicative
import Control.Monad
import Control.Monad.Fail as Fail
import Control.Monad.IO.Class

import Database.Daison.FFI
import Database.Daison.Serialize

-----------------------------------------------------------------
-- Access the database
-----------------------------------------------------------------

type Cookie = Word32
type Schema = Map.Map String (Int64,Int)
data Database = Database (Ptr Btree) (IORef (Cookie, Schema))

openDB :: String -> IO Database
openDB fpath = 
  withCString fpath $ \c_fpath ->
  alloca $ \ppBtree -> do
    checkSqlite3Error $ sqlite3BtreeOpen nullPtr c_fpath ppBtree 0 openFlags
    pBtree <- peek ppBtree
    schemaRef <- newIORef (-1,Map.empty)
    bracket_ (checkSqlite3Error $ sqlite3BtreeBeginTrans pBtree 0)
             (checkSqlite3Error $ sqlite3BtreeCommit pBtree)
             (fetchSchema pBtree schemaRef)
    return (Database pBtree schemaRef)

fetchSchema pBtree schemaRef = do
  (cookie,schema) <- readIORef schemaRef
  sqlite3BtreeLockTable pBtree 1 0
  cookie' <- alloca $ \pCookie -> do
               sqlite3BtreeGetMeta pBtree 1 pCookie
               peek pCookie
  if cookie == cookie'
    then return schema
    else do tables <- bracket (alloca $ \ppCursor -> do
                                 checkSqlite3Error $ sqlite3BtreeCursor pBtree 1 0 0 0 ppCursor
                                 peek ppCursor)
                              (\pCursor -> checkSqlite3Error $ sqlite3BtreeCloseCursor pCursor)
                              (\pCursor -> alloca $ \pRes -> do
                                             checkSqlite3Error $ sqlite3BtreeFirst pCursor pRes
                                             fetchEntries pCursor pRes)
            let schema = Map.fromList tables
            writeIORef schemaRef (cookie', schema)
            return schema
  where
    fetchEntries pCursor pRes = do
      res <- peek pRes
      if res == 0
        then do e <- fetchEntry pCursor
                checkSqlite3Error $ sqlite3BtreeNext pCursor pRes
                es <- fetchEntries pCursor pRes
                return (e:es)
        else return []

    fetchEntry pCursor = do
      key         <- alloca $ \pKey -> do
                       checkSqlite3Error $ sqlite3BtreeKeySize pCursor pKey
                       peek pKey
      (name,tnum) <- alloca $ \pSize -> do
                       checkSqlite3Error $ sqlite3BtreeDataSize pCursor pSize
                       peek pSize
                       ptr <- sqlite3BtreeDataFetch pCursor pSize
                       amt <- peek pSize
                       bs <- unsafePackCStringLen (castPtr ptr,fromIntegral amt)
                       return $! (deserialize bs)
      return (name,(key,tnum))

updateSchema pBtree schemaRef schema = do
  (cookie,_) <- readIORef schemaRef
  let cookie' = cookie+1
  checkSqlite3Error $ sqlite3BtreeUpdateMeta pBtree 1 cookie'
  writeIORef schemaRef (cookie',schema)

closeDB :: Database -> IO ()
closeDB (Database pBtree _) = do
  checkSqlite3Error $ sqlite3BtreeClose pBtree


-----------------------------------------------------------------
-- The monad
-----------------------------------------------------------------

newtype Daison a = Daison {doTransaction :: Database -> IO a}

data AccessMode = ReadWriteMode | ReadOnlyMode

toCMode ReadWriteMode = 1
toCMode ReadOnlyMode  = 0

runDaison :: Database -> AccessMode -> Daison a -> IO a
runDaison db@(Database pBtree schemaRef) m t =
  (do checkSqlite3Error $ sqlite3BtreeBeginTrans pBtree (toCMode m)
      r <- doTransaction t db
      checkSqlite3Error $ sqlite3BtreeCommit pBtree
      return r)
  `onException`
  (checkSqlite3Error $ sqlite3BtreeRollback pBtree sqlite_ABORT_ROLLBACK 0)

instance Functor Daison where
  fmap f (Daison m) = Daison (\db -> fmap f (m db))

instance Applicative Daison where
  pure x  = Daison (\db -> pure x)
  f <*> g = Daison (\db -> doTransaction f db <*> doTransaction g db)

instance Monad Daison where
#if !MIN_VERSION_base(4,13,0)
  fail msg = Daison (\db -> Fail.fail msg)
#endif
  return x = Daison (\db -> return x)
  f >>= g  = Daison (\db -> doTransaction f db >>= \x -> doTransaction (g x) db)

instance MonadFail Daison where
  fail msg = Daison (\db -> Fail.fail msg)

instance MonadIO Daison where
  liftIO f = Daison (\db -> f)

-----------------------------------------------------------------
-- Access the tables
-----------------------------------------------------------------

type Key a     = Int64
data Table a   = Table String [(String,a -> [ByteString])] [Ptr Btree -> Schema -> Key a -> IO ()]

table :: String -> Table a
table name = Table name [] []

tableName :: Table a -> String
tableName (Table name _ _) = name

createTable :: Table a -> Daison ()
createTable (Table name indices _) = Daison $ \db ->
  createTableHelper db btreeINTKEY name indices True

tryCreateTable :: Table a -> Daison ()
tryCreateTable (Table name indices _) = Daison $ \db ->
  createTableHelper db btreeINTKEY name indices False

createTableHelper (Database pBtree schemaRef) keyType name indices doFail = do
  schema <- fetchSchema pBtree schemaRef
  bracket (alloca $ \ppCursor -> do
             checkSqlite3Error $ sqlite3BtreeCursor pBtree 1 1 0 0 ppCursor
             peek ppCursor)
          (\pCursor -> checkSqlite3Error $ sqlite3BtreeCloseCursor pCursor)
          (\pCursor -> do res <- alloca $ \pRes -> do
                                   sqlite3BtreeLast pCursor pRes
                                   peek pRes
                          key <- if res /= 0
                                   then return 1
                                   else alloca $ \pKey -> do
                                          checkSqlite3Error $ sqlite3BtreeKeySize pCursor pKey
                                          fmap (+1) (peek pKey)
                          key_schema <- mkTable name keyType pCursor key schema
                          (key,schema) <- foldM (\(key,schema) (name,_) -> mkTable name btreeBLOBKEY pCursor key schema) key_schema indices
                          updateSchema pBtree schemaRef schema)
  where
    mkTable name flags pCursor key schema = do
      case Map.lookup name schema of
        Just _  -> if doFail
                     then throwAlreadyExists name
                     else return (key,schema)
        Nothing -> do tnum <- alloca $ \pTNum -> do
                                checkSqlite3Error $ sqlite3BtreeCreateTable pBtree pTNum flags
                                fmap fromIntegral (peek pTNum)
                      unsafeUseAsCStringLen (serialize (name,tnum)) $ \(ptr,size) -> do
                        sqlite3BtreeInsert pCursor nullPtr key (castPtr ptr) (fromIntegral size) 0 0 0
                      return (key+1,Map.insert name (key,tnum) schema)

dropTable :: Table a -> Daison ()
dropTable (Table name indices _) = Daison $ \db ->
  dropTableHelper db name indices True

tryDropTable :: Table a -> Daison ()
tryDropTable (Table name indices _) = Daison $ \db ->
  dropTableHelper db name indices False

dropTableHelper (Database pBtree schemaRef) name indices doFail = do
  schema <- fetchSchema pBtree schemaRef
  schema_ids   <- rmTable name (schema,[])
  (schema,ids) <- foldM (\schema_ids (name,_) -> rmTable name schema_ids) schema_ids indices
  bracket (alloca $ \ppCursor -> do
             checkSqlite3Error $ sqlite3BtreeCursor pBtree 1 1 0 0 ppCursor
             peek ppCursor)
          (\pCursor -> checkSqlite3Error $ sqlite3BtreeCloseCursor pCursor)
          (\pCursor -> mapM_ (rmSchema pCursor) ids)
  updateSchema pBtree schemaRef schema
  where
    rmTable name (schema,ids) =
      case Map.lookup name schema of
        Nothing         -> if doFail
                             then throwDoesn'tExist name
                             else return (schema,ids)
        Just (key,tnum) -> do alloca $ \piMoved -> do
                                checkSqlite3Error $ sqlite3BtreeDropTable pBtree (fromIntegral tnum) piMoved
                              return (Map.delete name schema,key:ids)

    rmSchema pCursor key = do
      alloca $ \pRes ->
        checkSqlite3Error $ sqlite3BtreeMoveTo pCursor nullPtr key 0 pRes
      checkSqlite3Error $ sqlite3BtreeDelete pCursor 0

alterTable :: (Data a, Data b) => Table a -> Table b -> (a -> b) -> Daison ()
alterTable tbl@(Table name _ _) (Table name' _ _) f = Daison $ \(Database pBtree schemaRef) -> do
  schema <- fetchSchema pBtree schemaRef
  case Map.lookup name schema of
    Nothing         -> throwDoesn'tExist name
    Just (key,tnum) -> do when (name /= name' && Map.member name' schema) $
                            throwAlreadyExists name'
                          tnum' <- alloca $ \pTNum -> do
                                     checkSqlite3Error $ sqlite3BtreeCreateTable pBtree pTNum btreeINTKEY
                                     fmap fromIntegral (peek pTNum)
                          sqlite3BtreeLockTable pBtree (fromIntegral tnum) 0
                          bracket (alloca $ \ppCursor -> do
                                     checkSqlite3Error $ sqlite3BtreeCursor pBtree (fromIntegral tnum) 0 0 0 ppCursor
                                     peek ppCursor)
                                  (\pCursor -> checkSqlite3Error $ sqlite3BtreeCloseCursor pCursor)
                                  (\pSrcCursor -> bracket (alloca $ \ppCursor -> do
                                                             checkSqlite3Error $ sqlite3BtreeCursor pBtree (fromIntegral tnum') 1 0 0 ppCursor
                                                             peek ppCursor)
                                                          (\pCursor -> checkSqlite3Error $ sqlite3BtreeCloseCursor pCursor)
                                                          (\pDstCursor -> step sqlite3BtreeFirst pSrcCursor pDstCursor))
                          bracket (alloca $ \ppCursor -> do
                                     checkSqlite3Error $ sqlite3BtreeCursor pBtree 1 1 0 0 ppCursor
                                     peek ppCursor)
                                  (\pCursor -> checkSqlite3Error $ sqlite3BtreeCloseCursor pCursor)
                                  (\pCursor -> unsafeUseAsCStringLen (serialize (name',tnum')) $ \(ptr,size) -> do
                                                 sqlite3BtreeInsert pCursor nullPtr key (castPtr ptr) (fromIntegral size) 0 0 0)
                          alloca $ \piMoved -> do
                            checkSqlite3Error $ sqlite3BtreeDropTable pBtree (fromIntegral tnum) piMoved
                          updateSchema pBtree schemaRef (Map.insert name' (key,tnum') (Map.delete name schema))
                          return ()
  where
    step moveCursor pSrcCursor pDstCursor = do
      res <- (alloca $ \pRes -> do
                checkSqlite3Error $ moveCursor pSrcCursor pRes
                peek pRes)
      if res /= 0
        then return ()
        else do key  <- alloca $ \pKey -> do
                          checkSqlite3Error $ sqlite3BtreeKeySize pSrcCursor pKey
                          peek pKey
                size <- alloca $ \pSize -> do
                          checkSqlite3Error $ sqlite3BtreeDataSize pSrcCursor pSize
                          peek pSize
                allocaBytes (fromIntegral size) $ \ptr -> do
                  checkSqlite3Error $ sqlite3BtreeData pSrcCursor 0 size ptr
                  bs <- unsafePackCStringLen (castPtr ptr,fromIntegral size)
                  let bs' = (serialize . f . deserialize) bs
                  unsafeUseAsCStringLen bs' $ \(ptr',size') -> do
                    checkSqlite3Error $ sqlite3BtreeInsert pDstCursor nullPtr key (castPtr ptr') (fromIntegral size') 0 0 0
                step sqlite3BtreeNext pSrcCursor pDstCursor

renameTable :: Table a -> String -> Daison (Table a)
renameTable (Table name indices triggers) name' = Daison $ \(Database pBtree schemaRef) -> do
  schema <- fetchSchema pBtree schemaRef
  case Map.lookup name schema of
    Nothing         -> throwDoesn'tExist name
    Just (key,tnum) -> do when (name /= name' && Map.member name' schema) $
                            throwAlreadyExists name'
                          bracket (alloca $ \ppCursor -> do
                                     checkSqlite3Error $ sqlite3BtreeCursor pBtree 1 1 0 0 ppCursor
                                     peek ppCursor)
                                  (\pCursor -> checkSqlite3Error $ sqlite3BtreeCloseCursor pCursor)
                                  (\pCursor -> unsafeUseAsCStringLen (serialize (name',tnum)) $ \(ptr,size) -> do
                                                 sqlite3BtreeInsert pCursor nullPtr key (castPtr ptr) (fromIntegral size) 0 0 0)
                          updateSchema pBtree schemaRef (Map.insert name' (key,tnum) (Map.delete name schema))
                          return (Table name' indices triggers)


openBtreeCursor pBtree schema name m n x =
  case Map.lookup name schema of
    Just (_,tnum) -> do sqlite3BtreeLockTable pBtree (fromIntegral tnum) (fromIntegral cmode)
                        alloca $ \ppCursor -> do
                          checkSqlite3Error $ sqlite3BtreeCursor pBtree (fromIntegral tnum) cmode n x ppCursor
                          peek ppCursor
    Nothing       -> throwDoesn'tExist name
  where
    cmode = toCMode m

closeBtreeCursor pCursor = checkSqlite3Error $ sqlite3BtreeCloseCursor pCursor


withTableCursor pBtree schema (Table name _ _) m io =
  bracket (openBtreeCursor pBtree schema name m 0 0)
          (closeBtreeCursor)
          io

withIndexCursors pBtree schema (Table _ indices _) m io =
  forM_ indices $ \(name,fn) ->
    bracket (openBtreeCursor pBtree schema name m 1 1)
            (closeBtreeCursor)
            (io fn)

withIndexCursor pBtree schema (Index tbl name fn) m io =
  bracket (openBtreeCursor pBtree schema name m 1 1)
          (closeBtreeCursor)
          (io (map serialize . fn))

runTriggers pBtree schema (Table _ _ ts) key = forM_ ts (\t -> t pBtree schema key)

-----------------------------------------------------------------
-- Access the indices
-----------------------------------------------------------------

data    Index a b = Index (Table a) String (a -> [b])

index :: Table a -> String -> (a -> b) -> Index a b
index tbl iname f = listIndex tbl iname ((:[]) . f)

listIndex :: Table a -> String -> (a -> [b]) -> Index a b
listIndex ~tbl@(Table tname indices _) iname = Index tbl (tname++"_"++iname)

maybeIndex :: Table a -> String -> (a -> Maybe b) -> Index a b
maybeIndex tbl iname f = listIndex tbl iname (maybeToList . f)

withIndex :: Data b => Table a -> Index a b -> Table a
withIndex (Table tname indices fkeys) (Index _ iname fn) = Table tname ((iname,map serialize . fn) : indices) fkeys

indexedTable :: Index a b -> Table a
indexedTable (Index tbl _ _) = tbl

applyIndex :: Index a b -> a -> [b]
applyIndex (Index _ _ fn) = fn

-----------------------------------------------------------------
-- Foreign Keys
-----------------------------------------------------------------

withForeignKey :: Table a -> Index b (Key a) -> Table a
withForeignKey t@(Table tname indices fkeys) index = Table tname indices (check:fkeys)
  where
    check pBtree schema key = do
      bs <- fromAt' return index (At key) pBtree schema
      unless (Data.ByteString.null bs) $
        throwIO (DatabaseError ("The foreign key from "++tableName (indexedTable index)++" to "++tname++" is violated"))

withCascadeDelete :: Data b => Table a -> Index b (Key a) -> Table a
withCascadeDelete (Table tname indices fkeys) index = Table tname indices (cascade:fkeys)
  where
    cascade pBtree schema key =
      withTableCursor pBtree schema tbl ReadWriteMode $ \pCursor ->
        fromAt' (deleteForeign pCursor) index (At key) pBtree schema
      where
        tbl = indexedTable index

        deleteForeign pCursor bs =
          case deserializeKey bs of
            Nothing       -> do return ()
            Just (key,bs) -> do res <- alloca $ \pRes -> do
                                         checkSqlite3Error $ sqlite3BtreeMoveTo pCursor nullPtr key 0 pRes
                                         peek pRes
                                when (res == 0) $ do
                                  runTriggers pBtree schema tbl key
                                  deleteIndices pBtree schema tbl key pCursor
                                  checkSqlite3Error $ sqlite3BtreeDelete pCursor 0
                                deleteForeign pCursor bs

withCascadeUpdate :: Data b => Table a -> (Index b (Key a), Key a -> b -> b) -> Table a
withCascadeUpdate (Table tname indices fkeys) (index,f) = Table tname indices (cascade:fkeys)
  where
    cascade pBtree schema key =
      withTableCursor pBtree schema tbl ReadWriteMode $ \pCursor ->
        fromAt' (updateForeign pCursor) index (At key) pBtree schema
      where
        tbl = indexedTable index

        updateForeign pCursor bs =
          case deserializeKey bs of
            Nothing       -> do return ()
            Just (key,bs) -> do res <- alloca $ \pRes -> do
                                         checkSqlite3Error $ sqlite3BtreeMoveTo pCursor nullPtr key 0 pRes
                                         peek pRes
                                when (res == 0) $ do
                                  val <- deleteIndices pBtree schema tbl key pCursor
                                  let val' = f key val
                                  unsafeUseAsCStringLen (serialize val') $ \(ptr,size) ->
                                    checkSqlite3Error $ sqlite3BtreeInsert pCursor nullPtr key (castPtr ptr) (fromIntegral size) 0 0 0
                                  insertIndices pBtree schema tbl key val'
                                updateForeign pCursor bs

-----------------------------------------------------------------
-- Access the Data Sets
-----------------------------------------------------------------

newtype DataSet a = DataSet {datasetName :: String}

dataset = DataSet

createDataSet :: DataSet a -> Daison ()
createDataSet (DataSet name) = Daison $ \db ->
  createTableHelper db btreeBLOBKEY name [] True

tryCreateDataSet :: DataSet a -> Daison ()
tryCreateDataSet (DataSet name) = Daison $ \db ->
  createTableHelper db btreeBLOBKEY name [] False

dropDataSet :: DataSet a -> Daison ()
dropDataSet (DataSet name) = Daison $ \db ->
  dropTableHelper db name [] True

tryDropDataSet :: DataSet a -> Daison ()
tryDropDataSet (DataSet name) = Daison $ \db ->
  dropTableHelper db name [] False

renameDataSet :: DataSet a -> String -> Daison (DataSet a)
renameDataSet (DataSet name) name' = Daison $ \(Database pBtree schemaRef) -> do
  schema <- fetchSchema pBtree schemaRef
  case Map.lookup name schema of
    Nothing         -> throwDoesn'tExist name
    Just (key,tnum) -> do when (name /= name' && Map.member name' schema) $
                            throwAlreadyExists name'
                          bracket (alloca $ \ppCursor -> do
                                     checkSqlite3Error $ sqlite3BtreeCursor pBtree 1 1 0 0 ppCursor
                                     peek ppCursor)
                                  (\pCursor -> checkSqlite3Error $ sqlite3BtreeCloseCursor pCursor)
                                  (\pCursor -> unsafeUseAsCStringLen (serialize (name',tnum)) $ \(ptr,size) -> do
                                                 sqlite3BtreeInsert pCursor nullPtr key (castPtr ptr) (fromIntegral size) 0 0 0)
                          updateSchema pBtree schemaRef (Map.insert name' (key,tnum) (Map.delete name schema))
                          return (DataSet name')

-----------------------------------------------------------------
-- Queries
-----------------------------------------------------------------

newtype Query a = Query {doQuery :: Ptr Btree -> Schema -> IO (QSeq a)}

data QSeq a
  = Output a (IO (QSeq a)) (IO ())
  | Done

nilQSeq = return Done
done    = return ()
close pCursor = sqlite3BtreeCloseCursor pCursor >> return ()

instance Functor Query where
  fmap f q = Query (\pBtree schema -> fmap mapSeq (doQuery q pBtree schema))
             where mapSeq (Output a m d) = Output (f a) (fmap mapSeq m) d
                   mapSeq Done           = Done

instance Applicative Query where
  pure x  = Query (\pBtree schema -> pure (Output x nilQSeq done))
  f <*> g = Query (\pBtree schema -> doQuery f pBtree schema  >>= loop pBtree schema )
    where
      loop pBtree schema Done                = return Done
      loop pBtree schema (Output f f' done0) = doQuery g pBtree schema  >>= append
        where
          append Done                 = f' >>= loop pBtree schema
          append (Output x rest done) = return (Output (f x) (rest >>= append) (done0 >> done))

instance Alternative Query where
  empty   = mzero
  f <|> g = mplus f g

instance Monad Query where
#if !MIN_VERSION_base(4,13,0)
  fail _    = mzero
#endif
  return x  = Query (\pBtree schema  -> return (Output x nilQSeq done))
  f >>= g   = Query (\pBtree schema  -> doQuery f pBtree schema  >>= loop pBtree schema)
    where
      loop pBtree schema Done                = return Done
      loop pBtree schema (Output x f' done0) = doQuery (g x) pBtree schema >>= append 
        where
          append Done                 = f' >>= loop pBtree schema
          append (Output x rest done) = return (Output x (rest >>= append) (done0 >> done))

instance MonadFail Query where
  fail _    = mzero

instance MonadPlus Query where
  mzero     = Query (\pBtree schema -> return Done)
  mplus f g = Query (\pBtree schema -> 
    let append Done                 = doQuery g pBtree schema
        append (Output x rest done) = return (Output x (rest >>= append) done)
    in doQuery f pBtree schema >>= append)

select :: QueryMonad m => Query a -> m [a]
select = query listRows

class QueryMonad m where
  query  :: Aggregator a b -> Query a -> m b

instance QueryMonad Daison where
  query aggr q = Daison $ \(Database pBtree schemaRef) -> do
    schema <- fetchSchema pBtree schemaRef
    runAggregator aggr (doQuery q pBtree schema)

instance QueryMonad Query where
  query aggr q = Query $ \pBtree schema -> do
    rs <- runAggregator aggr (doQuery q pBtree schema)
    return (Output rs nilQSeq done)

anyOf :: [a] -> Query a
anyOf xs = Query (\pBtree schema -> loop xs)
  where loop []     = return Done
        loop (x:xs) = return (Output x (loop xs) done)

-----------------------------------------------------------------
-- Aggregators
-----------------------------------------------------------------

newtype Aggregator a b = Aggregator {runAggregator :: IO (QSeq a) -> IO b}

instance Functor (Aggregator a) where
  fmap f a = Aggregator (\r -> fmap f (runAggregator a r))

listRows :: Aggregator a [a]
listRows = Aggregator (\r -> r >>= loop)
  where
    loop Done           = return []
    loop (Output x r _) = do xs <- r >>= loop
                             return (x:xs)

distinctRows :: Ord a => Aggregator a (Set.Set a)
distinctRows = Aggregator (\r -> r >>= loop)
  where
    loop Done           = return Set.empty
    loop (Output x r _) = do xs <- r >>= loop
                             return $! (Set.insert x xs)

firstRow :: Aggregator a a
firstRow = Aggregator (\r -> r >>= loop)
  where
    loop Done           = Fail.fail "first: empty sequence"
    loop (Output y r d) = d >> return y

lastRow :: Aggregator a a
lastRow = Aggregator (\r -> do mb_x <- r >>= loop
                               case mb_x of
                                 Nothing -> Fail.fail "last: empty sequence"
                                 Just x  -> return x)
  where
    loop Done           = return Nothing
    loop (Output y r d) = do mb_x <- r >>= loop
                             case mb_x of
                               Nothing -> return (Just y)
                               Just x  -> return (Just x)

topRows :: Int -> Aggregator a [a]
topRows n = Aggregator (\r -> if n > 0
                                then r >>= loop n
                                else return [])
  where
    loop n Done           = return []
    loop n (Output x r d)
      | n == 1            = d >> return [x]
      | otherwise         = do xs <- r >>= loop (n-1)
                               return (x:xs)

bottomRows :: Int -> Aggregator a [a]
bottomRows n = Aggregator (\r -> if n > 0
                                   then fmap snd (r >>= loop)
                                   else return [])
  where
    loop Done           = return (0,[])
    loop (Output x r d) = do
      (m,xs) <- r >>= loop
      if m == n
        then return (m,    xs)
        else return (m+1,x:xs)

foldRows :: (b -> a -> b) -> b -> Aggregator a b
foldRows f x = Aggregator (\r -> r >>= loop x)
  where
    loop !x Done           = return x
    loop !x (Output y r d) = r >>= loop (f x y)

foldRows1 :: (a -> a -> a) -> Aggregator a a
foldRows1 f = Aggregator (\r -> r >>= first)
  where
    first Done           = Fail.fail "foldRows1: empty sequence"
    first (Output x r _) = r >>= loop x

    loop !x Done           = return x
    loop !x (Output y r _) = r >>= loop (f x y)

sumRows :: Num a => Aggregator a a
sumRows = foldRows (+) 0

averageRows :: Fractional a => Aggregator a a
averageRows = fmap (\(s,c) -> s/c)
                   (foldRows (\(s,c) x -> (s+x,c+1)) (0,0))

countRows :: Aggregator a Int
countRows = foldRows (\c x -> c+1) 0

groupRows :: Ord a => Aggregator (a,b) (Map.Map a [b])
groupRows = groupRowsWith (:) []

groupRowsWith :: Ord a => (b -> c -> c) -> c -> Aggregator (a,b) (Map.Map a c)
groupRowsWith f z = Aggregator (\r -> r >>= loop Map.empty)
  where
    loop m Done           = return m
    loop m (Output x r _) = r >>= (loop $! add x m)

    add  x = Map.alter (Just . f (snd x) . fromMaybe z) (fst x)

groupRowsBy :: Ord b => (a -> b) -> Aggregator a (Map.Map b [a])
groupRowsBy f = Aggregator (\r -> r >>= loop Map.empty)
  where
    loop m Done           = return m
    loop m (Output x r _) = r >>= (loop $! add x m)

    add  x = Map.alter (Just . (x:) . fromMaybe []) (f x)

groupRowsByWith :: Ord b => (a -> b) -> (a -> c -> c) -> c -> Aggregator a (Map.Map b c)
groupRowsByWith f g z = Aggregator (\r -> r >>= loop Map.empty)
  where
    loop m Done           = return m
    loop m (Output x r _) = r >>= (loop $! add x m)

    add  x = Map.alter (Just . g x . fromMaybe z) (f x)

having :: Functor m => m (Map.Map a b) -> (b -> Bool) -> m (Map.Map a b)
having f p = fmap (Map.filter p) f

sortRows :: Ord a => Aggregator a [a]
sortRows     = fmap List.sort listRows

sortRowsBy :: (a -> a -> Ordering) -> Aggregator a [a]
sortRowsBy f = fmap (List.sortBy f) listRows

-----------------------------------------------------------------
-- Select
-----------------------------------------------------------------

data Restriction a
  = Restriction (IntervalBoundry a) (IntervalBoundry a) Order

data Order
  = Asc | Desc

data IntervalBoundry a
  = Including a
  | Excluding a
  | Unbounded

everything = asc

asc  = Restriction Unbounded Unbounded Asc
desc = Restriction Unbounded Unbounded Desc

infixl 4 ^>=, ^>, ^<=, ^<

(^>=) :: Restriction a -> a -> Restriction a
(Restriction s e Asc ) ^>= x = Restriction (Including x) e Asc
(Restriction s e Desc) ^>= x = Restriction s (Including x) Desc

(^>)  :: Restriction a -> a -> Restriction a
(Restriction s e Asc ) ^> x = Restriction (Excluding x) e Asc
(Restriction s e Desc) ^> x = Restriction s (Excluding x) Desc

(^<=) :: Restriction a -> a -> Restriction a
(Restriction s e Asc ) ^<= x = Restriction s (Including x) Asc
(Restriction s e Desc) ^<= x = Restriction (Including x) e Desc

(^<)  :: Restriction a -> a -> Restriction a
(Restriction s e Asc ) ^< x = Restriction s (Excluding x) Asc
(Restriction s e Desc) ^< x = Restriction (Excluding x) e Desc



newtype At a = At a

at = At


class From s (r :: * -> *) where
  type K s
  type V s r

  from :: s -> r (K s) -> Query (V s r)


instance Data a => From (Table a) At where
  type K (Table a)    = Key a
  type V (Table a) At = a

  from tbl (At key) = Query $ \pBtree schema ->
    withTableCursor pBtree schema tbl ReadOnlyMode $ \pCursor -> do
      res <- alloca $ \pRes -> do
               checkSqlite3Error $ sqlite3BtreeMoveTo pCursor nullPtr key 0 pRes
               peek pRes
      if res /= 0
        then return Done
        else do alloca $ \pSize -> do
                  checkSqlite3Error $ sqlite3BtreeDataSize pCursor pSize
                  size <- peek pSize
                  ptr <- mallocBytes (fromIntegral size)
                  bs <- unsafePackMallocCStringLen (castPtr ptr,fromIntegral size)
                  checkSqlite3Error $ sqlite3BtreeData pCursor 0 size ptr
                  return (Output (deserialize bs) nilQSeq done)

instance Data a => From (Table a) Restriction where
  type K (Table a)             = Key a
  type V (Table a) Restriction = (Key a,a)

  from (Table name _ _) (Restriction s e order) = Query $ \pBtree schema -> do
    pCursor <- openBtreeCursor pBtree schema name ReadOnlyMode 0 0
    step (start s) pCursor
    where
      start (Including key) pCursor pRes = do
        rc <- sqlite3BtreeMoveTo pCursor nullPtr key 0 pRes
        case order of
          Asc  -> do when (rc == sqlite_OK) $ do res <- peek pRes
                                                 when (res > 0) $ poke pRes 0
                     return rc
          Desc -> do if rc == sqlite_OK
                       then do res <- peek pRes
                               if res > 0 
                                 then sqlite3BtreePrevious pCursor pRes
                                 else return rc
                       else return rc
      start (Excluding key) pCursor pRes = do
        rc <- sqlite3BtreeMoveTo pCursor nullPtr key 0 pRes
        if rc == sqlite_OK
          then do res <- peek pRes
                  case order of
                    Asc  | res == 0 -> sqlite3BtreeNext pCursor pRes
                    Desc | res >= 0 -> sqlite3BtreePrevious pCursor pRes
                    _               -> return rc
          else return rc
      start Unbounded pCursor pRes =
        case order of
          Asc  -> sqlite3BtreeFirst pCursor pRes
          Desc -> sqlite3BtreeLast  pCursor pRes

      moveNext pCursor pRes = do
        rc <- case order of
                Asc  -> sqlite3BtreeNext     pCursor pRes
                Desc -> sqlite3BtreePrevious pCursor pRes
        if rc /= sqlite_OK
          then return rc
          else alloca $ \pKey -> do
                 rc <- sqlite3BtreeKeySize pCursor pKey
                 if rc /= sqlite_OK
                   then return rc
                   else do key' <- peek pKey
                           case (order,e) of
                             (Asc, Including key) | key' >  key -> poke pRes 1
                             (Asc, Excluding key) | key' >= key -> poke pRes 1
                             (Desc,Including key) | key' <  key -> poke pRes (-1)
                             (Desc,Excluding key) | key' <= key -> poke pRes (-1)
                             _                                  -> return ()
                           return rc

      step moveCursor pCursor = do
        res <- (alloca $ \pRes -> do
                  checkSqlite3Error $ moveCursor pCursor pRes
                  peek pRes)
               `onException`
               (sqlite3BtreeCloseCursor pCursor)
        if res /= 0
          then do sqlite3BtreeCloseCursor pCursor
                  return Done
          else (do key  <- alloca $ \pKey -> do
                             checkSqlite3Error $ sqlite3BtreeKeySize pCursor pKey
                             peek pKey
                   size <- alloca $ \pSize -> do
                             checkSqlite3Error $ sqlite3BtreeDataSize pCursor pSize
                             peek pSize
                   ptr  <- mallocBytes (fromIntegral size)
                   checkSqlite3Error $ sqlite3BtreeData pCursor 0 size ptr
                   bs <- unsafePackMallocCStringLen (castPtr ptr,fromIntegral size)
                   return (Output (key, deserialize bs) (step moveNext pCursor) (close pCursor)))
               `onException`
               (sqlite3BtreeCloseCursor pCursor)


instance Data b => From (Index a b) At where
  type K (Index a b)    = b
  type V (Index a b) At = Key a
  
  from idx x = Query (fromAt' deserializeKeys idx x)
    where
      deserializeKeys bs =
        case deserializeKey bs of
          Nothing       -> return Done
          Just (key,bs) -> return (Output key (deserializeKeys bs) done)

instance Data b => From (Index a b) Restriction where
  type K (Index a b)             = b
  type V (Index a b) Restriction = (b,Key a)

  from (Index tbl name fn) x = Query (fromInterval' deserialize name x)
    where
      deserialize bs next done =
        let (val,bs') = deserializeIndex bs
        in deserializeKeys val bs'
        where
          deserializeKeys val bs =
            case deserializeKey bs of
              Nothing       -> next
              Just (key,bs) -> return (Output (val,key) (deserializeKeys val bs) done)

instance Data a => From (DataSet a) Restriction where
  type K (DataSet a)             = a
  type V (DataSet a) Restriction = a

  from (DataSet name) x = Query (fromInterval' deserialize name x)
    where
      deserialize bs next done =
        let (val,bs') = deserializeIndex bs
        in return (Output val next done)

class FromIndex (r :: * -> *) where
  type VI r a b
  type VL r a b

  fromIndex :: (Data a,Data b) => Index a b -> r b -> Query (VI r a b)
  fromList  :: (Data a,Data b) => Index a b -> r b -> Query (VL r a b)

instance FromIndex At where
  type VI At a b = (Key a,a)
  type VL At a b = [Key a]

  fromIndex idx r = do key <- from idx r
                       val <- from (indexedTable idx) (at key)
                       return (key,val)

  fromList idx x = Query (fromAt' deserialize idx x)
    where
      deserialize bs = return (Output (deserializeKeys bs) (return Done) done)

      deserializeKeys bs =
        case deserializeKey bs of
          Nothing       -> []
          Just (key,bs) -> key : deserializeKeys bs

instance FromIndex Restriction where
  type VI Restriction a b = (Key a,a,b)
  type VL Restriction a b = (b,[Key a])

  fromIndex idx r = do (ival,key) <- from idx r
                       dval       <- from (indexedTable idx) (at key)
                       return (key,dval,ival)

  fromList (Index tbl name fn) x = Query (fromInterval' deserialize name x)
    where
      deserialize bs next done = do
        let (val,bs') = deserializeIndex bs
        return (Output (val,deserializeKeys bs') next done)
        where
          deserializeKeys bs =
            case deserializeKey bs of
              Nothing       -> []
              Just (key,bs) -> key : deserializeKeys bs

fromAt' deserialize idx (At val) = \pBtree schema ->
    withIndexCursor pBtree schema idx ReadOnlyMode $ \fn pCursor ->
    unsafeUseAsCStringLen (serialize val) $ \(indexPtr,indexSize) -> do
      res <- alloca $ \pRes -> do
               checkSqlite3Error $ sqlite3BtreeMoveTo pCursor (castPtr indexPtr) (fromIntegral indexSize) 0 pRes
               peek pRes
      if res /= 0
        then deserialize Data.ByteString.empty
        else do alloca $ \pSize -> do
                  checkSqlite3Error $ sqlite3BtreeKeySize pCursor pSize
                  payloadSize <- peek pSize
                  let size = fromIntegral payloadSize-indexSize
                  ptr <- mallocBytes size
                  bs <- unsafePackMallocCStringLen (castPtr ptr,fromIntegral size)
                  checkSqlite3Error $ sqlite3BtreeKey pCursor (fromIntegral indexSize) (fromIntegral size) ptr
                  deserialize bs

fromInterval' deserialize name (Restriction s e order) = \pBtree schema -> do
  pCursor <- openBtreeCursor pBtree schema name ReadOnlyMode 1 1
  step (start s) pCursor
  where
    ebs = case e of
            Including val -> serialize val
            Excluding val -> serialize val
            Unbounded     -> error "No end value"

    start (Including val) pCursor pRes =
      unsafeUseAsCStringLen (serialize val) $ \(indexPtr,indexSize) -> do
        rc <- sqlite3BtreeMoveTo pCursor (castPtr indexPtr) (fromIntegral indexSize) 0 pRes
        case order of
          Asc  -> do when (rc == sqlite_OK) $ do res <- peek pRes
                                                 when (res > 0) $ poke pRes 0
                     return rc
          Desc -> do if rc == sqlite_OK
                       then do res <- peek pRes
                               if res > 0 
                                 then sqlite3BtreePrevious pCursor pRes
                                 else return rc
                       else return rc
    start (Excluding val) pCursor pRes =
      unsafeUseAsCStringLen (serialize val) $ \(indexPtr,indexSize) -> do
        rc <- sqlite3BtreeMoveTo pCursor (castPtr indexPtr) (fromIntegral indexSize) 0 pRes
        if rc == sqlite_OK
          then do res <- peek pRes
                  case order of
                    Asc  | res == 0 -> sqlite3BtreeNext     pCursor pRes
                    Desc | res >= 0 -> sqlite3BtreePrevious pCursor pRes
                    _               -> return rc
          else return rc
    start Unbounded       pCursor pRes =
      case order of
        Asc  -> sqlite3BtreeFirst pCursor pRes
        Desc -> sqlite3BtreeLast  pCursor pRes

    next =
      case order of
        Asc  -> sqlite3BtreeNext
        Desc -> sqlite3BtreePrevious

    checkEnd (Including _) pCursor nCell pCell f =
      unsafeUseAsCStringLen ebs $ \(pKey,_) -> do
      alloca $ \pRC -> do
        res <- sqlite3BtreeRecordCompare nCell pCell (castPtr pKey) pRC
        checkSqlite3Error $ peek pRC
        case order of
          Asc  | res <= 0 -> f pCursor
          Desc | res >= 0 -> f pCursor
          _               -> do sqlite3BtreeCloseCursor pCursor
                                return Done
    checkEnd (Excluding _) pCursor nCell pCell f =
      unsafeUseAsCStringLen ebs $ \(pKey,_) -> do
      alloca $ \pRC -> do
        res <- sqlite3BtreeRecordCompare nCell pCell (castPtr pKey) pRC
        checkSqlite3Error $ peek pRC
        case order of
          Asc  | res <  0 -> f pCursor
          Desc | res >  0 -> f pCursor
          _               -> do sqlite3BtreeCloseCursor pCursor
                                return Done
    checkEnd Unbounded     pCursor nCell pCell f = f pCursor

    step moveCursor pCursor = do
      res <- (alloca $ \pRes -> do
                checkSqlite3Error $ moveCursor pCursor pRes
                peek pRes)
             `onException`
             (sqlite3BtreeCloseCursor pCursor)
      if res /= 0
        then do sqlite3BtreeCloseCursor pCursor
                return Done
        else (alloca $ \pSize -> do
                checkSqlite3Error $ sqlite3BtreeKeySize pCursor pSize
                payloadSize <- peek pSize
                ptr <- mallocBytes (fromIntegral payloadSize)
                bs <- unsafePackMallocCStringLen (castPtr ptr,fromIntegral payloadSize)
                checkSqlite3Error $ sqlite3BtreeKey pCursor 0 (fromIntegral payloadSize) ptr
                checkEnd e pCursor payloadSize ptr $ \pCursor -> do
                  deserialize bs (step next pCursor) (close pCursor))
             `onException`
             (sqlite3BtreeCloseCursor pCursor)


-----------------------------------------------------------------
-- Insert
-----------------------------------------------------------------

insert :: Data a => Table a -> Query a -> Daison (Key a,Key a)
insert tbl q = Daison $ \(Database pBtree schemaRef) -> do
  schema <- fetchSchema pBtree schemaRef
  withTableCursor pBtree schema tbl ReadWriteMode $ \pCursor -> do
    res <- alloca $ \pRes -> do
             sqlite3BtreeLast pCursor pRes
             peek pRes
    key <- if res /= 0
             then return 1
             else alloca $ \pKey -> do
                    checkSqlite3Error $ sqlite3BtreeKeySize pCursor pKey
                    fmap (+1) (peek pKey)
    seq <- doQuery q pBtree schema
    key' <- loop pBtree schema pCursor key seq
    return (key,key')
  where
    loop pBtree schema pCursor key Done             = return key
    loop pBtree schema pCursor key (Output val r _) = do unsafeUseAsCStringLen (serialize val) $ \(ptr,size) -> do
                                                           checkSqlite3Error $ sqlite3BtreeInsert pCursor nullPtr key (castPtr ptr) (fromIntegral size) 0 0 0
                                                         insertIndices pBtree schema tbl key val
                                                         r >>= loop pBtree schema pCursor (key+1)

insert_ :: Data a => Table a -> a -> Daison (Key a)
insert_ tbl = store tbl Nothing

insertIndices pBtree schema tbl key val =
  unsafeUseAsCStringLen (serializeKey key) $ \(keyPtr,keySize) -> do
    withIndexCursors pBtree schema tbl ReadWriteMode $ \fn pCursor ->
      forM_ (fn val) $ \bs ->
      unsafeUseAsCStringLen bs $ \(indexPtr,indexSize) -> do
        res <- alloca $ \pRes -> do
                 checkSqlite3Error $ sqlite3BtreeMoveTo pCursor (castPtr indexPtr) (fromIntegral indexSize) 0 pRes
                 peek pRes
        if res == 0
          then do payloadSize <- alloca $ \pPayloadSize -> do
                                   checkSqlite3Error $ sqlite3BtreeKeySize pCursor pPayloadSize
                                   peek pPayloadSize
                  allocaBytes (fromIntegral payloadSize+keySize) $ \buf -> do
                    checkSqlite3Error $ sqlite3BtreeKey pCursor 0 (fromIntegral payloadSize) buf
                    copyBytes (buf `plusPtr` fromIntegral payloadSize) keyPtr keySize
                    checkSqlite3Error $ sqlite3BtreeInsert pCursor buf (fromIntegral (fromIntegral payloadSize+keySize)) nullPtr 0 0 0 0 
          else allocaBytes (indexSize+keySize) $ \buf -> do
                 copyBytes buf indexPtr indexSize
                 copyBytes (buf `plusPtr` indexSize) keyPtr keySize
                 checkSqlite3Error $ sqlite3BtreeInsert pCursor (castPtr buf) (fromIntegral (indexSize+keySize)) nullPtr 0 0 0 0
  

-----------------------------------------------------------------
-- Store
-----------------------------------------------------------------

store :: Data a => Table a -> Maybe (Key a) -> a -> Daison (Key a)
store tbl mb_key val = Daison $ \(Database pBtree schemaRef) -> do
  schema <- fetchSchema pBtree schemaRef
  key <- withTableCursor pBtree schema tbl ReadWriteMode $ \pCursor -> do
           key <- alloca $ \pRes -> do
                    case mb_key of
                      Nothing  -> do sqlite3BtreeLast pCursor pRes
                                     res <- peek pRes
                                     if res /= 0
                                       then return 1
                                       else alloca $ \pKey -> do
                                              checkSqlite3Error $ sqlite3BtreeKeySize pCursor pKey
                                              fmap (+1) (peek pKey)
                      Just key -> do checkSqlite3Error $ sqlite3BtreeMoveTo pCursor nullPtr key 0 pRes
                                     res <- peek pRes
                                     when (res == 0) (deleteIndices pBtree schema tbl key pCursor >> return ())
                                     return key
           unsafeUseAsCStringLen (serialize val) $ \(ptr,size) -> do
             checkSqlite3Error $ sqlite3BtreeInsert pCursor nullPtr key (castPtr ptr) (fromIntegral size) 0 0 0
           return key
  insertIndices pBtree schema tbl key val
  return key

-----------------------------------------------------------------
-- Insert in a DataSet
-----------------------------------------------------------------

insertData :: Data a => DataSet a -> a -> Daison ()
insertData (DataSet name) val = Daison $ \(Database pBtree schemaRef) -> do
  schema <- fetchSchema pBtree schemaRef
  bracket (openBtreeCursor pBtree schema name ReadWriteMode 1 1)
          (closeBtreeCursor)
          (\pCursor -> unsafeUseAsCStringLen (serialize val) $ \(valPtr,valSize) -> do
             res <- alloca $ \pRes -> do
                      checkSqlite3Error $ sqlite3BtreeMoveTo pCursor (castPtr valPtr) (fromIntegral valSize) 0 pRes
                      peek pRes
             when (res /= 0) $
               checkSqlite3Error $ sqlite3BtreeInsert pCursor (castPtr valPtr) (fromIntegral valSize) nullPtr 0 0 0 0)

-----------------------------------------------------------------
-- Update
-----------------------------------------------------------------

update :: Data a => Table a -> Query (Key a,a) -> Daison [(Key a,a)]
update tbl q = Daison $ \(Database pBtree schemaRef) -> do
  schema <- fetchSchema pBtree schemaRef
  withTableCursor pBtree schema tbl ReadWriteMode $ \pCursor -> do
     seq <- doQuery q pBtree schema
     loop pBtree schema pCursor seq
  where
    loop pBtree schema pCursor Done                   = return []
    loop pBtree schema pCursor (Output p@(key,x) r _) = do res <- alloca $ \pRes -> do
                                                                    checkSqlite3Error $ sqlite3BtreeMoveTo pCursor nullPtr key 0 pRes
                                                                    peek pRes
                                                           when (res == 0) (deleteIndices pBtree schema tbl key pCursor >> return ())
                                                           unsafeUseAsCStringLen (serialize x) $ \(ptr,size) -> do
                                                             checkSqlite3Error $ sqlite3BtreeInsert pCursor nullPtr key (castPtr ptr) (fromIntegral size) 0 0 0
                                                           insertIndices pBtree schema tbl key x
                                                           ps <- r >>= loop pBtree schema pCursor
                                                           return (p:ps)

update_ :: Data a => Table a -> Query (Key a,a) -> Daison ()
update_ tbl q = Daison $ \(Database pBtree schemaRef) -> do
  schema <- fetchSchema pBtree schemaRef
  withTableCursor pBtree schema tbl ReadWriteMode $ \pCursor -> do
     seq <- doQuery q pBtree schema
     loop pBtree schema pCursor seq
  where
    loop pBtree schema pCursor Done                 = return ()
    loop pBtree schema pCursor (Output (key,x) r _) = do res <- alloca $ \pRes -> do
                                                                  checkSqlite3Error $ sqlite3BtreeMoveTo pCursor nullPtr key 0 pRes
                                                                  peek pRes
                                                         when (res == 0) (deleteIndices pBtree schema tbl key pCursor >> return ())
                                                         unsafeUseAsCStringLen (serialize x) $ \(ptr,size) -> do
                                                           checkSqlite3Error $ sqlite3BtreeInsert pCursor nullPtr key (castPtr ptr) (fromIntegral size) 0 0 0
                                                         insertIndices pBtree schema tbl key x
                                                         r >>= loop pBtree schema pCursor

-----------------------------------------------------------------
-- Delete
-----------------------------------------------------------------

delete :: Data a => Table a -> Query (Key a) -> Daison [Key a]
delete tbl q = Daison $ \db@(Database pBtree schemaRef) -> do
  schema <- fetchSchema pBtree schemaRef
  withTableCursor pBtree schema tbl ReadWriteMode $ \pCursor -> do
    seq <- doQuery q pBtree schema
    loop pBtree schema pCursor seq
  where
    loop pBtree schema pCursor Done             = return []
    loop pBtree schema pCursor (Output key r _) = do 
      res <- alloca $ \pRes -> do
               checkSqlite3Error $ sqlite3BtreeMoveTo pCursor nullPtr key 0 pRes
               peek pRes
      if res == 0
        then do runTriggers pBtree schema tbl key
                deleteIndices pBtree schema tbl key pCursor
                checkSqlite3Error $ sqlite3BtreeDelete pCursor 0
                keys <- r >>= loop pBtree schema pCursor
                return (key:keys)
        else do r >>= loop pBtree schema pCursor

delete_ :: Data a => Table a -> Query (Key a) -> Daison ()
delete_ tbl q = Daison $ \(Database pBtree schemaRef) -> do
  schema <- fetchSchema pBtree schemaRef
  withTableCursor pBtree schema tbl ReadWriteMode $ \pCursor -> do
     seq <- doQuery q pBtree schema
     loop pBtree schema pCursor seq
  where
    loop pBtree schema pCursor Done             = return ()
    loop pBtree schema pCursor (Output key r _) = do 
      res <- alloca $ \pRes -> do
               checkSqlite3Error $ sqlite3BtreeMoveTo pCursor nullPtr key 0 pRes
               peek pRes
      when (res == 0) $ do
        runTriggers pBtree schema tbl key
        deleteIndices pBtree schema tbl key pCursor
        checkSqlite3Error $ sqlite3BtreeDelete pCursor 0
      r >>= loop pBtree schema pCursor

deleteIndices pBtree schema tbl key pCursor = do
  size <- alloca $ \pSize -> do
    checkSqlite3Error $ sqlite3BtreeDataSize pCursor pSize
    peek pSize
  ptr <- mallocBytes (fromIntegral size)
  bs <- unsafePackMallocCStringLen (castPtr ptr,fromIntegral size)
  checkSqlite3Error $ sqlite3BtreeData pCursor 0 size ptr
  let val = deserialize bs
  withIndexCursors pBtree schema tbl ReadWriteMode $ \fn pCursor ->
    forM_ (fn val) $ \bsKey ->
      unsafeUseAsCStringLen bsKey $ \(indexPtr,indexSize) -> do
        res <- alloca $ \pRes -> do
                 checkSqlite3Error $ sqlite3BtreeMoveTo pCursor (castPtr indexPtr) (fromIntegral indexSize) 0 pRes
                 peek pRes
        if res /= 0
          then return ()
          else alloca $ \pSize -> do
                  checkSqlite3Error $ sqlite3BtreeKeySize pCursor pSize
                  payloadSize <- peek pSize
                  let size = fromIntegral payloadSize-indexSize
                  ptr <- mallocBytes size
                  bs <- unsafePackMallocCStringLen (castPtr ptr,fromIntegral size)
                  checkSqlite3Error $ sqlite3BtreeKey pCursor (fromIntegral indexSize) (fromIntegral size) ptr
                  case List.delete key (deserializeKeys bs) of
                    [] -> checkSqlite3Error $ sqlite3BtreeDelete pCursor 0
                    ks -> unsafeUseAsCStringLen (bsKey `append` serializeKeys ks) $ \(indexPtr,indexSize) -> do
                            checkSqlite3Error $ sqlite3BtreeInsert pCursor (castPtr indexPtr) (fromIntegral indexSize) nullPtr 0 0 0 0
  return val
  where
    deserializeKeys bs =
      case deserializeKey bs of
        Nothing       -> []
        Just (key,bs) -> key : deserializeKeys bs


-----------------------------------------------------------------
-- Delete from DataSet
-----------------------------------------------------------------

deleteData :: Data a => DataSet a -> a -> Daison ()
deleteData (DataSet name) val = Daison $ \db@(Database pBtree schemaRef) -> do
  schema <- fetchSchema pBtree schemaRef
  bracket (openBtreeCursor pBtree schema name ReadWriteMode 1 1)
          (closeBtreeCursor)
          (\pCursor -> unsafeUseAsCStringLen (serialize val) $ \(valPtr,valSize) -> do
             res <- alloca $ \pRes -> do
                      checkSqlite3Error $ sqlite3BtreeMoveTo pCursor (castPtr valPtr) (fromIntegral valSize) 0 pRes
                      peek pRes
             when (res == 0) $
               checkSqlite3Error $ sqlite3BtreeDelete pCursor 0)


-----------------------------------------------------------------
-- Exceptions
-----------------------------------------------------------------

newtype DatabaseError = DatabaseError String
     deriving (Show, Typeable)

instance Exception DatabaseError

checkSqlite3Error m = do
  rc <- m
  if rc /= sqlite_OK
    then do msg <- sqlite3BtreeErrName rc >>= peekCString
            throwIO (DatabaseError msg)
    else return ()

throwAlreadyExists name =
  throwIO (DatabaseError ("Table "++name++" already exists"))

throwDoesn'tExist name =
  throwIO (DatabaseError ("Table "++name++" does not exist"))

