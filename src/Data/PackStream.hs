{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Data.PackStream
(
  PackStreamError (..), PackStream (..), PackStreamValue (..)
, unpackStream, unpackFail, unpackThrow
, Value (..), (=:)
, Structure (..)
) where

import Data.PackStream.Internal.Type
import qualified Data.PackStream.Parser as P
import qualified Data.PackStream.Serializer as S

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Map.Strict (Map)
import Control.Monad.Except (MonadError(..), liftEither)

#if !MIN_VERSION_base(4, 13, 0)
import Control.Monad.Fail (MonadFail)
#endif

-- |The data types that can be interpreted or parsed to/from 'PackStream' 'ByteString'
class PackStreamValue a where
    -- |Pack a value into a 'PackStream' 'ByteString'
    pack :: a -> ByteString
    -- |Parse a value from a 'PackStream' 'ByteString'
    unpack :: PackStream a

instance PackStreamValue () where
    pack _ = S.null
    unpack = P.null

instance PackStreamValue Bool where
    pack = S.bool
    unpack = P.bool

instance PackStreamValue Int where
    pack = S.integer
    unpack = P.integer

instance PackStreamValue Integer where
    pack = S.integer . fromIntegral
    unpack = fromIntegral <$> P.integer

instance PackStreamValue Double where
    pack = S.float
    unpack = P.float

instance PackStreamValue ByteString where
    pack = S.bytes
    unpack = P.bytes

instance PackStreamValue Text where
    pack = S.string
    unpack = P.string

instance (ToValue a, PackStreamValue a) => PackStreamValue [a] where
    pack = S.list . fmap toValue
    unpack = P.list unpack

instance (ToValue a, PackStreamValue a) => PackStreamValue (Map Text a) where
    pack = S.dict . fmap toValue
    unpack = P.dict unpack

instance PackStreamValue Structure where
    pack = S.structure
    unpack = P.structure

instance PackStreamValue Value where
    pack = S.value
    unpack = P.value

-- |Unpack some value of the specific type from 'ByteString' or raise 'PackStreamError'
unpackThrow :: (MonadError PackStreamError m, PackStreamValue a) => ByteString -> m a
unpackThrow = liftEither . unpackStream unpack

-- |Unpack some value of the specific type from 'ByteString' or 'fail'
unpackFail :: (MonadFail m, PackStreamValue a) => ByteString -> m a
unpackFail bs = case unpackStream unpack bs of
                  Right x -> pure x
                  Left  e -> fail $ show e