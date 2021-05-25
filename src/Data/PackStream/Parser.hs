{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.PackStream.Parser
(
  null, bool, integer, float,
  bytes, string, list, dict, structure,
  value
) where

import Data.PackStream.Internal.Type ( PackStream, PackStreamError(..), Structure(..), Value(..) )
import Data.PackStream.Internal.Code
import Data.PackStream.Internal.Binary ( Interpret(..) )

import Prelude hiding (null)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (head, tail, drop, take, singleton)
import Data.Text (Text)
import Data.Map.Strict (Map, fromList)
import Data.Binary (Word8)
import Control.Monad.State ( gets, modify )
import Control.Monad.Except (throwError)
import Control.Monad (replicateM, (>=>))


-- |Parse '()'
null :: PackStream ()
null = bite1 >>= select NotNull [ (isNull, pure ()) ]

-- |Parse 'Bool'
bool :: PackStream Bool
bool = bite1 >>= select NotBool [ (isTrue, pure True)
                                , (isFalse, pure False)
                                ]

-- |Parse 'Int'
integer :: PackStream Int
integer = bite1 >>= \x -> select NotInt [ (isTinyInt, interpret $ BS.singleton x)
                                        , (isInt8,  bite 1 >>= interpret)
                                        , (isInt16, bite 2 >>= interpret)
                                        , (isInt32, bite 4 >>= interpret)
                                        , (isInt64, bite 8 >>= interpret)
                                        ] x

-- |Parse 'Double'
float :: PackStream Double
float = bite1 >>= select NotFloat [ (isFloat, bite 8 >>= interpret) ]

-- |Parse 'ByteString'
bytes :: PackStream ByteString
bytes = bite1 >>= select NotBytes [ (isBytes8,  bite'by'bytes 1)
                                  , (isBytes16, bite'by'bytes 2)
                                  , (isBytes32, bite'by'bytes 4)
                                  ]

-- |Parse 'Text'
string :: PackStream Text
string = bite1 >>= \x -> select NotBytes [ (isTinyString, bite (tinySize x) >>= interpret)
                                         , (isString8,    bite'by'bytes 1 >>= interpret)
                                         , (isString16,   bite'by'bytes 2 >>= interpret)
                                         , (isString32,   bite'by'bytes 4 >>= interpret)
                                         ] x

-- |Parse a list of specified 'Value's (e.g. `list integer` will parse some '[Int]')
list :: PackStream a -> PackStream [a]
list action = bite1 >>= \x -> select NotList [ (isTinyList, multiple action (tinySize x))
                                             , (isList8,    collectionSize 1 >>= multiple action)
                                             , (isList16,   collectionSize 2 >>= multiple action)
                                             , (isList32,   collectionSize 4 >>= multiple action)
                                             ] x

-- |Parse a dictionary of specified 'Value's (e.g. `dict integer` will parse some 'Map Text Int')
dict :: forall a.PackStream a -> PackStream (Map Text a)
dict action = bite1 >>= \x -> select NotDict [ (isTinyDict, makeDict (tinySize x))
                                             , (isDict8,    collectionSize 1 >>= makeDict)
                                             , (isDict16,   collectionSize 2 >>= makeDict)
                                             , (isDict32,   collectionSize 4 >>= makeDict)
                                             ] x
  where
    makeDict :: Int -> PackStream (Map Text a)
    makeDict = (fromList <$>) . multiple ((,) <$> string <*> action)

-- |Parse 'Structure'
structure :: PackStream Structure
structure = bite1 >>= \x -> select NotStructure [ (isStructure, makeStructure (tinySize x)) ] x
  where
    makeStructure :: Int -> PackStream Structure
    makeStructure n = Structure <$> bite1 <*> multiple value n

-- |Parse any valid 'Value'
value :: PackStream Value
value = look1 >>= byMarker
  where
    byMarker :: Word8 -> PackStream Value
    byMarker n | isNull n      = N <$ null
               | isBool n      = B <$> bool
               | isInt n       = I <$> integer
               | isFloat n     = F <$> float
               | isBytes n     = U <$> bytes
               | isString n    = T <$> string
               | isList n      = L <$> list value
               | isDict n      = D <$> dict value
               | isStructure n = S <$> structure
               | otherwise     = throwError NotValue

-- |Selects a parser to use by the marker byte predicate. Raises the 'PackStreamError' if nothing is suitable
select :: PackStreamError -> [(Word8 -> Bool, PackStream a)] -> Word8 -> PackStream a
select e []               _ = throwError e
select e ((p, action):xs) w | p w       = action
                            | otherwise = select e xs w

-- |Gets one byte from the 'ByteString'
bite1 :: PackStream Word8
bite1 = do b <- gets BS.head
           modify BS.tail
           pure b

-- |Gets the specified number of bytes from the 'ByteString'
bite :: Int -> PackStream ByteString
bite n = do bs <- gets $ BS.take n
            modify $ BS.drop n
            pure bs

-- |Looks at the first byte of the 'ByteString' without modifying it
look1 :: PackStream Word8
look1 = gets BS.head

-- |Gets the specified number of bytes from the 'ByteString', 
-- interprets them as some unsigned int ('Word') and then get the specified number of
-- further bytes from the 'ByteString'
bite'by'bytes :: Int -> PackStream ByteString
bite'by'bytes n = collectionSize n >>= bite

-- |Performs some 'PackStream' parser combinator the specified number of times
multiple :: PackStream a -> Int -> PackStream [a]
multiple action n = replicateM n action

-- |Gets the specified number of bytes from the 'ByteString' and
-- interprets them as some unsigned int ('Word')
collectionSize :: Int -> PackStream Int
collectionSize n = bite n >>= fmap fromIntegral . interpret @Word