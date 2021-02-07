{-# LANGUAGE BinaryLiterals #-}
module Database.Daison.Serialize(serialize,deserialize,
                                 serializeKey, serializeKeys,
                                 deserializeKey, deserializeIndex) where

import Data.Int(Int64)
import Data.Bits
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Generics
import Data.ByteString(ByteString)
import Data.ByteString.Lazy(fromStrict,toStrict)
import Control.Monad

data Tag = Tag {-# UNPACK #-} !Word8 {-# UNPACK #-} !Int String

tag_list     = Tag     0b00 2 "a list"
tag_str      = Tag     0b10 2 "a string"
tag_int      = Tag     0b01 2 "an int"
tag_con      = Tag    0b011 3 "a constructor"
tag_end      = Tag  0b00111 5 "an args-end"
tag_float    = Tag  0b01111 5 "a float"
tag_double   = Tag  0b10111 5 "a double"
tag_rational = Tag  0b11111 5 "a rational"
tag_char     = Tag 0b100111 6 "a char"
tag_int_list = Tag 0b101111 6 "an integer list"
tag_flt_list = Tag 0b110111 6 "a list of floats"
tag_dbl_list = Tag 0b111111 6 "a list of doubles"

serialize :: Data a => a -> ByteString
serialize = toStrict . runPut . serializeM

serializeM :: Data a => a -> Put
serializeM =
  serializeDefault `ext1Q`
  serializeDataList `extQ`
  (serializeBinaryList tag_str      :: String   -> Put) `extQ`
  (serializeBinaryList tag_int_list :: [Int]    -> Put) `extQ`
  (serializeBinaryList tag_flt_list :: [Float]  -> Put) `extQ`
  (serializeBinaryList tag_dbl_list :: [Double] -> Put) `extQ`
  serializeFloat `extQ`
  serializeDouble
  where
    serializeDefault t =
      case constrRep (toConstr t) of
        AlgConstr i   -> putVInt (fromIntegral i) tag_con >>
                         gmapQr (>>) (return ()) serializeM t >>
                         putTag tag_end
        IntConstr i   -> putVInt i tag_int
        FloatConstr r -> putTag tag_rational >> put r
        CharConstr  c -> putTag tag_char  >> put c

    serializeBinaryList :: Binary a => Tag -> [a] -> Put
    serializeBinaryList tag s = do
      putVInt (fromIntegral (length s)) tag
      mapM_ put s

    serializeDataList :: Data a => [a] -> Put
    serializeDataList xs = do
      putVInt (fromIntegral (length xs)) tag_list
      mapM_ serializeM xs

    serializeFloat :: Float -> Put
    serializeFloat f = putTag tag_float >> putFloatbe f

    serializeDouble :: Double -> Put
    serializeDouble f = putTag tag_double >> putDoublebe f

putTag :: Tag -> Put
putTag (Tag tag tbits _) = putWord8 tag

putVInt :: Integer -> Tag -> Put
putVInt n (Tag tag tbits _) =
  let rbits = 7-tbits
      n0    = fromIntegral (n .&. (1 `shiftL` rbits - 1)) `shiftL` (tbits+1)
      n'    = n `shiftR` rbits
  in if (n' == 0 && (n0 .&. 0x80 == 0)) || (n' == -1 && (n0 .&. 0x80 /= 0))
       then do putWord8 (n0 .|. tag)
       else do putWord8 (n0 .|. (1 `shiftL` tbits) .|. tag)
               putRest n'

serializeKey :: Int64 -> ByteString
serializeKey = toStrict . runPut . putRest . fromIntegral

serializeKeys :: [Int64] -> ByteString
serializeKeys = toStrict . runPut . mapM_ (putRest . fromIntegral)

-- specialized version without tag bits
putRest :: Integer -> Put
putRest n =
  let n0 = fromIntegral (n .&. (1 `shiftL` 7 - 1)) `shiftL` 1
      n' = n `shiftR` 7
  in if (n' == 0 && (n0 .&. 0x80 == 0)) || (n' == -1 && (n0 .&. 0x80 /= 0))
       then putWord8 n0
       else do putWord8 (n0 .|. 1)
               putRest n'

deserialize :: Data a => ByteString -> a
deserialize = runGet deserializeM . fromStrict

deserializeM :: Data a => Get a
deserializeM =
  deserializeDefault `ext1R`
  deserializeDataList `extR`
  (deserializeBinaryList tag_str :: Get String) `extR`
  (deserializeBinaryList tag_int_list :: Get [Int]) `extR`
  (deserializeBinaryList tag_flt_list :: Get [Float]) `extR`
  (deserializeBinaryList tag_dbl_list :: Get [Double]) `extR`
  deserializeChar `extR`
  deserializeFloat `extR`
  deserializeDouble
  where
    deserializeDefault = do
      (con,f) <- getConstr                    -- get the constructor
      x       <- fromConstrM deserializeM con -- Read the children
      when f (getTag tag_end)
      return x

    deserializeDataList :: Data a => Get [a]
    deserializeDataList = do
      len <- getVInt tag_list
      getList len
      where
        getList 0 = return []
        getList n = do
          c  <- deserializeM
          cs <- getList (n-1)
          return (c:cs)

    deserializeBinaryList :: Binary a => Tag -> Get [a]
    deserializeBinaryList tag = do
      len <- getVInt tag
      getList len
      where
        getList 0 = return []
        getList n = do
          c  <- get
          cs <- getList (n-1)
          return (c:cs)

    deserializeChar :: Get Char
    deserializeChar = getTag tag_char >> get

    deserializeFloat :: Get Float
    deserializeFloat = getTag tag_float >> getFloatbe

    deserializeDouble :: Get Double
    deserializeDouble = getTag tag_double >> getDoublebe
    
    deserializeRational :: Get Rational
    deserializeRational = getTag tag_rational >> get

    myDataType = dataTypeOf (getArg deserializeDefault)
      where
        getArg :: Get a'' -> a''
        getArg = undefined

    getConstr :: Get (Constr,Bool)
    getConstr =
      do i <- getVInt tag_con
         if i <= fromIntegral (maxConstrIndex myDataType)
           then return (indexConstr myDataType (fromIntegral i),True)
           else fail ("the data type has no constructor with index "++show i)
      `mplus`
      do i <- getVInt tag_int
         return (mkIntegralConstr myDataType i,False)
      `mplus`
      do getTag tag_rational
         r <- get
         return (mkRealConstr myDataType (r :: Rational),False)
      `mplus`
      do getTag tag_char
         c <- get
         return (mkCharConstr myDataType c,False)


getTag :: Tag -> Get ()
getTag (Tag tag bits name) = do
  w <- getWord8
  if w .&. (1 `shiftL` bits - 1) == tag
    then return ()
    else fail ("failed to find "++name)

getVInt :: Tag -> Get Integer
getVInt (Tag tag bits name) = do
  w <- getWord8
  if w .&. (1 `shiftL` bits - 1) == tag
    then let n = fromIntegral (w `shiftR` (bits+1))
         in if w .&. (1 `shiftL` bits) == 0
              then if w .&. 0x80 /= 0
                     then return (n .|. ((-1) `shiftL` (7-bits)))
                     else return n
              else getRest (7-bits) n
    else fail ("failed to find "++name)

getRest :: Int -> Integer -> Get Integer
getRest bits n = do
  w <- getWord8
  let n' = n .|. (fromIntegral (w .&. 0xFE) `shiftL` (bits-1))
  if w .&. 1 == 0
    then if w .&. 0x80 /= 0
           then return (n' .|. ((-128) `shiftL` bits))
           else return n'
    else getRest (bits+7) n'

deserializeKey :: ByteString -> Maybe (Int64,ByteString)
deserializeKey bs =
  feed (runGetIncremental (getRest 0 0)) (Just bs)
  where
    feed (Fail _ pos msg)  mb_bs = Nothing
    feed (Partial k)       mb_bs = feed (k mb_bs) Nothing
    feed (Done bs pos key) mb_bs = Just (fromIntegral key,bs)

deserializeIndex :: Data a => ByteString -> (a,ByteString)
deserializeIndex bs =
  feed (runGetIncremental deserializeM) (Just bs)
  where
    feed (Fail _ pos msg)  mb_bs = error ("Database.Helda.deserializeIndex at position " ++ show pos ++ ": " ++ msg)
    feed (Partial k)       mb_bs = feed (k mb_bs) Nothing
    feed (Done bs pos val) mb_bs = (val,bs)
