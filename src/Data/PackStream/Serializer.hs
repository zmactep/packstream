{-# LANGUAGE TypeApplications #-}
module Data.PackStream.Serializer
(
  null, bool, integer, float,
  bytes, string, list, dict, structure,
  value
) where


import Data.PackStream.Internal.Type (Structure (..), Value (..))
import Data.PackStream.Internal.Code
import Data.PackStream.Internal.Binary (Serialize(..), inDepth)

import Prelude hiding (null)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (singleton, cons, append, length, concat, empty)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Map.Strict (Map, toList)
import Data.Word (Word8)


-- |Represent '()' as 'PackStream' 'ByteString'
null :: ByteString
null = BS.singleton nullCode

-- |Represent 'Bool' as 'PackStream' 'ByteString'
bool :: Bool -> ByteString
bool False = BS.singleton falseCode
bool True  = BS.singleton trueCode

-- |Represent 'Int' as 'PackStream' 'ByteString'
integer :: Int -> ByteString
integer n | n >= -16 && n < 128 = serialize n
          | inDepth 8  n        = int8Code  `BS.cons` serialize n
          | inDepth 16 n        = int16Code `BS.cons` serialize n
          | inDepth 32 n        = int32Code `BS.cons` serialize n
          | inDepth 64 n        = int64Code `BS.cons` serialize n
          | otherwise           = BS.empty

-- |Represent 'Double' as 'PackStream' 'ByteString'
float :: Double -> ByteString
float x = floatCode `BS.cons` serialize x

-- |Represent 'ByteString' as 'PackStream' 'ByteString'
bytes :: ByteString -> ByteString
bytes bs = constructCollection mstart bs
  where
    mstart :: Maybe ByteString
    mstart = (`BS.cons` serializeLen (BS.length bs)) <$> firstByte
  
    firstByte :: Maybe Word8
    firstByte | BS.length bs <= 0xFF       = pure bytes8Code
              | BS.length bs <= 0xFFFF     = pure bytes16Code
              | BS.length bs <= 0xFFFFFFFF = pure bytes32Code
              | otherwise                  = Nothing

-- |Represent 'Text' as 'PackStream' 'ByteString'
string :: Text -> ByteString
string s = constructCollection mstart (serialize s)
  where
    mstart :: Maybe ByteString
    mstart = firstBytes stringTinyCode string8Code string16Code string32Code (BS.length $ encodeUtf8 s)

-- |Represent '[Value]' as 'PackStream' 'ByteString'
list :: [Value] -> ByteString
list l = constructCollection mstart (BS.concat (value <$> l))
  where
    mstart :: Maybe ByteString
    mstart = firstBytes listTinyCode list8Code list16Code list32Code (length l)

-- |Represent 'Map Text Value' as 'PackStream' 'ByteString'
dict :: Map Text Value -> ByteString
dict d = constructCollection mstart (BS.concat (kvs <$> toList d))
  where
    mstart :: Maybe ByteString
    mstart = firstBytes dictTinyCode dict8Code dict16Code dict32Code (length d)

    kvs :: (Text, Value) -> ByteString
    kvs (k, v) = string k `BS.append` value v

-- |Represent 'Structure' as 'PackStream' 'ByteString'
structure :: Structure -> ByteString
structure s | len < 16  = structureCode + len `BS.cons` signature s `BS.cons` BS.concat (value <$> fields s)
            | otherwise = BS.empty
  where
    len = fromIntegral $ length $ fields s

-- |Represent 'Value' as 'PackStream' 'ByteString'
value :: Value -> ByteString
value N     = null
value (B b) = bool b
value (I i) = integer i
value (F f) = float f
value (U u) = bytes u
value (T t) = string t
value (L l) = list l
value (D d) = dict d
value (S s) = structure s

-- Helper functions

constructCollection :: Maybe ByteString -> ByteString -> ByteString
constructCollection mstart end = case mstart of
                                   Just start -> start `BS.append` end
                                   Nothing    -> BS.empty

firstBytes :: Word8 -> Word8 -> Word8 -> Word8 -> Int -> Maybe ByteString
firstBytes bt b8 b16 b32 len | len <= 0xF        = pure $ BS.singleton $ bt  + fromIntegral len
                             | len <= 0xFF       = pure $ b8  `BS.cons` serializeLen len
                             | len <= 0xFFFF     = pure $ b16 `BS.cons` serializeLen len
                             | len <= 0xFFFFFFFF = pure $ b32 `BS.cons` serializeLen len
                             | otherwise         = Nothing

serializeLen :: Int -> ByteString
serializeLen = serialize @Word . fromIntegral