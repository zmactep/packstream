{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeApplications #-}
module Data.PackStream.Internal.Type where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State (State, MonadState, evalState)
import Data.ByteString (ByteString)
import Data.Binary (Word8)
import Data.Map.Strict (Map)
import Data.Text (Text)

-- * PackStream basics

-- PackStream is a general purpose data serialisation format, originally inspired 
-- by (but incompatible with) MessagePack. This module provides basic types
-- and typeclasses to help you to parse or to serialize.

-- |Basic 'PackStream' error type that is used to handle parsing errors.
data PackStreamError = NotNull             -- ^This 'ByteString' doesn't represent null object
                     | NotBool             -- ^This 'ByteString' doesn't represent any boolean
                     | NotWord             -- ^This 'ByteString' doesn't represent any unsigned integer
                     | NotInt              -- ^This 'ByteString' doesn't represent any integer
                     | NotFloat            -- ^This 'ByteString' doesn't represent any floating-point number
                     | NotString           -- ^This 'ByteString' doesn't represent any 'Text' string
                     | NotBytes            -- ^This 'ByteString' doesn't represent any 'ByteString' array
                     | NotList             -- ^This 'ByteString' doesn't represent any list of 'PackStream' values
                     | NotDict             -- ^This 'ByteString' doesn't represent any dictionary of 'PackStream' values
                     | NotStructure        -- ^This 'ByteString' doesn't represent any 'Structure'
                     | NotValue            -- ^This 'ByteString' doesn't represent any 'Value'
                     | WrongStructure Text -- ^This 'ByteString' doesn't represent specific 'Structure'
                     | DictHasNoKey Text   -- ^The dictionary doesn't have a specified 'Text' key
  deriving (Show, Eq, Ord)

-- |Basic parser type. It works like parser combinators for binary data that represents PackStream.
newtype PackStream a = PackStream { runUnpackS :: ExceptT PackStreamError (State ByteString) a }
  deriving (Functor, Applicative, Monad, MonadState ByteString, MonadError PackStreamError)

-- |Use specific parser combinator to parse the 'ByteString' that represents any 'PackStream' data.
unpackStream :: PackStream a -> ByteString -> Either PackStreamError a
unpackStream action = evalState (runExceptT $ runUnpackS action)

-- |PackStream offers a number of core data types, many supported by multiple binary representations, as well as a flexible extension mechanism.
data Value = N                   -- ^Missing or empty value
           | B Bool              -- ^True or False
           | I Int               -- ^Signed 64-bit integer
           | F Double            -- ^64-bit floating point number
           | U ByteString        -- ^Byte array
           | T Text              -- ^Unicode text, UTF-8
           | L [Value]           -- ^Ordered collection of 'Value's
           | D (Map Text Value)  -- ^Collection of key-value entries (no order guaranteed)
           | S Structure         -- ^Composite value with a type signature
  deriving (Show, Eq)

-- |A structure is a composite value, comprised of fields and a unique type code.
-- Structure encodings consist, beyond the marker, of a single byte, the tag byte, followed 
-- by a sequence of up to 15 fields, each an individual value. The size of a structure is 
-- measured as the number of fields and not the total byte size. This count does not include the tag.
data Structure = Structure { signature :: Word8   -- ^Type code
                           , fields    :: [Value] -- ^Structure fields
                           }
  deriving (Show, Eq)

-- |The data types that can be serialized as 'PackStream'
class ToValue a where
    -- |Convert data type to the generic 'Value'
    toValue :: a -> Value

instance ToValue () where
    toValue = const N

instance ToValue Bool where
    toValue = B

instance ToValue Int where
    toValue = I

instance ToValue Integer where
    toValue = toValue @Int . fromIntegral

instance ToValue Double where
    toValue = F

instance ToValue ByteString where
    toValue = U

instance ToValue Text where
    toValue = T

instance ToValue a => ToValue [a] where
    toValue = L . fmap toValue

instance ToValue a => ToValue (Map Text a) where
    toValue = D . fmap toValue

instance ToValue Structure where
    toValue = S

instance ToValue Value where
    toValue = id

-- |Represent a 'Text' key and some 'ToValue' data into the 'Map' pair.
-- Can be useful to work with 'PackStream' dictionaries.
--
-- > fromList ["hello" =: 1, "world" =: False]
(=:) :: ToValue a => Text -> a -> (Text, Value)
(=:) key val = (key, toValue val)

-- |The data types taht can be read from 'PackStream' representation
class FromValue a where
    -- |Converts generic 'Value' type to a specific one or raises 'PackStreamError'
    fromValue :: Value -> Either PackStreamError a

instance FromValue () where
    fromValue N = pure ()
    fromValue _ = throwError NotNull

instance FromValue Bool where
    fromValue (B x) = pure x
    fromValue _     = throwError NotBool

instance FromValue Int where
    fromValue (I x) = pure x
    fromValue _     = throwError NotInt

instance FromValue Integer where
    fromValue = fmap fromIntegral . fromValue @Int

instance FromValue Double where
    fromValue (F x) = pure x
    fromValue _     = throwError NotFloat

instance FromValue ByteString where
    fromValue (U x) = pure x
    fromValue _     = throwError NotBytes

instance FromValue Text where
    fromValue (T x) = pure x
    fromValue _     = throwError NotString

instance FromValue a => FromValue [a] where
    fromValue (L xs) = traverse fromValue xs
    fromValue _      = throwError NotList

instance FromValue a => FromValue (Map Text a) where
    fromValue (D mp) = traverse fromValue mp
    fromValue _      = throwError NotDict

instance FromValue Structure where
    fromValue (S x) = pure x
    fromValue _     = throwError NotStructure

instance FromValue Value where
    fromValue = pure