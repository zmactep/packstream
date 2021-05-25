module Data.PackStream.Internal.Code where

import Data.Binary (Word8)
import Control.Applicative (liftA2)
import Data.Bits ((.&.))

-- * Checkers
--
-- These functions check the marker bytes of 'ByteString's to
-- identify the represented datatype. There names are self-describing.

-- |Checks whether the represented data is '()'
isNull :: Word8 -> Bool
isNull = (== nullCode)

isFalse, isTrue :: Word8 -> Bool
isFalse = (== falseCode)
isTrue = (== trueCode)

-- |Checks whether the represented data is 'Bool'
isBool :: Word8 -> Bool
isBool = isSmth [isFalse, isTrue]

isTinyInt, isInt8, isInt16, isInt32, isInt64 :: Word8 -> Bool
isTinyInt = liftA2 (||) (>= 0xF0) (<= 0x7F)
isInt8 = (== int8Code)
isInt16 = (== int16Code)
isInt32 = (== int32Code)
isInt64 = (== int64Code)

-- |Checks whether the represented data is 'Int'
isInt :: Word8 -> Bool
isInt = isSmth [isTinyInt, isInt8, isInt16, isInt32, isInt64]

-- |Checks whether the represented data is 'Double'
isFloat :: Word8 -> Bool
isFloat = (== floatCode)

isBytes8, isBytes16, isBytes32 :: Word8 -> Bool
isBytes8  = (== bytes8Code)
isBytes16 = (== bytes16Code)
isBytes32 = (== bytes32Code)

-- |Checks whether the represented data is 'ByteString'
isBytes :: Word8 -> Bool
isBytes = isSmth [isBytes8, isBytes16, isBytes32]

isTinyString, isString8, isString16, isString32 :: Word8 -> Bool
isTinyString = liftA2 (&&) (>= stringTinyCode) (<= stringTinyCode + 0x0F)
isString8 = (== string8Code)
isString16 = (== string16Code)
isString32 = (== string32Code)

-- |Checks whether the represented data is 'Text'
isString :: Word8 -> Bool
isString = isSmth [isTinyString, isString8, isString16, isString32]

isTinyList, isList8, isList16, isList32 :: Word8 -> Bool
isTinyList = liftA2 (&&) (>= listTinyCode) (<= listTinyCode + 0x0F)
isList8 = (== list8Code)
isList16 = (== list16Code)
isList32 = (== list32Code)

-- |Checks whether the represented data is '[Value]'
isList :: Word8 -> Bool
isList = isSmth [isTinyList, isList8, isList16, isList32]

isTinyDict, isDict8, isDict16, isDict32 :: Word8 -> Bool
isTinyDict = liftA2 (&&) (>= dictTinyCode) (<= dictTinyCode + 0x0F)
isDict8 = (== dict8Code)
isDict16 = (== dict16Code)
isDict32 = (== dict32Code)

-- |Checks whether the represented data is 'Map Text Value'
isDict :: Word8 -> Bool
isDict = isSmth [isTinyDict, isDict8, isDict16, isDict32]

-- |Checks whether the represented data is 'Structure'
isStructure :: Word8 -> Bool
isStructure = liftA2 (&&) (>= structureCode) (<= structureCode + 0x0F)

-- * Helper functions

-- |Extracts the size of tiny collection ('Text', '[Value]' or 'Map Text Value')
tinySize :: Word8 -> Int
tinySize x = fromIntegral $ x .&. 0x0F

-- |Gets the collection of predicates and checks whether any is 'True' on some data
isSmth :: (Foldable t, Functor t) => t (b -> Bool) -> b -> Bool
isSmth preds = or . flip fmap preds . flip id

-- * Codes
--
-- These are the 'PackStream' constants that mark specific data types.

nullCode :: Word8
nullCode = 0xC0

falseCode, trueCode :: Word8
falseCode = 0xC2
trueCode = 0xC3

int8Code, int16Code, int32Code, int64Code :: Word8
int8Code = 0xC8
int16Code = 0xC9
int32Code = 0xCA
int64Code = 0xCB

floatCode :: Word8
floatCode = 0xC1

bytes8Code, bytes16Code, bytes32Code :: Word8
bytes8Code = 0xCC
bytes16Code = 0xCD
bytes32Code = 0xCE

stringTinyCode, string8Code, string16Code, string32Code :: Word8
stringTinyCode = 0x80
string8Code = 0xD0
string16Code = 0xD1
string32Code = 0xD2

listTinyCode, list8Code, list16Code, list32Code :: Word8
listTinyCode = 0x90
list8Code = 0xD4
list16Code = 0xD5
list32Code = 0xD6

dictTinyCode, dict8Code, dict16Code, dict32Code :: Word8
dictTinyCode = 0xA0
dict8Code = 0xD8
dict16Code = 0xD9
dict32Code = 0xDA

structureCode :: Word8
structureCode = 0xB0