{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.PackStream.Internal.Binary where

import Data.PackStream.Internal.Type ( PackStream, PackStreamError(..) )

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (length)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Binary (Binary, encode, decode)
import Data.Binary.IEEE754 (wordToDouble, doubleToWord)
import Data.Text (Text)
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Control.Applicative (liftA2)
import Data.Word (Word, Word8, Word16, Word32, Word64)
import Control.Monad.Except (throwError, MonadError)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)


-- |The data types that can be intepreted from 'ByteString'
class Interpret a where
    -- |Interpret a 'ByteString' as a specific type of raise 'PackStreamError'
    interpret :: MonadError PackStreamError m => ByteString -> m a

    default interpret :: (MonadError PackStreamError m, Binary a) => ByteString -> m a
    interpret = pure . decode . fromStrict

instance Interpret Int8
instance Interpret Int16
instance Interpret Int32
instance Interpret Int64

instance Interpret Word8
instance Interpret Word16
instance Interpret Word32
instance Interpret Word64

instance Interpret Word where
    interpret bs | BS.length bs == 1 = fromIntegral @Word8  <$> interpret bs
                 | BS.length bs == 2 = fromIntegral @Word16 <$> interpret bs
                 | BS.length bs == 4 = fromIntegral @Word32 <$> interpret bs
                 | BS.length bs == 8 = fromIntegral @Word64 <$> interpret bs
                 | otherwise         = throwError NotWord

instance Interpret Int where
    interpret bs | BS.length bs == 1 = fromIntegral @Int8  <$> interpret bs
                 | BS.length bs == 2 = fromIntegral @Int16 <$> interpret bs
                 | BS.length bs == 4 = fromIntegral @Int32 <$> interpret bs
                 | BS.length bs == 8 = fromIntegral @Int64 <$> interpret bs
                 | otherwise         = throwError NotInt

instance Interpret Integer where
    interpret = fmap fromIntegral . interpret @Int

instance Interpret Double where
    interpret = fmap wordToDouble . interpret

instance Interpret ByteString where
    interpret = pure

instance Interpret Text where
    interpret = pure . decodeUtf8

-- |The data types that can be serialized into 'ByteString'
class Serialize a where
    -- |Serialize a specific data type into a 'ByteString'
    serialize :: a -> ByteString

    default serialize :: Binary a => a -> ByteString
    serialize = toStrict . encode

instance Serialize Int8
instance Serialize Int16
instance Serialize Int32
instance Serialize Int64

instance Serialize Word8
instance Serialize Word16
instance Serialize Word32
instance Serialize Word64

instance Serialize Word where
    serialize i | i <= 0xFF               = serialize @Word8  $ fromIntegral i
                | i <= 0xFFFF             = serialize @Word16 $ fromIntegral i
                | i <= 0xFFFFFFFF         = serialize @Word32 $ fromIntegral i
                | i <= 0xFFFFFFFFFFFFFFFF = serialize @Word64 $ fromIntegral i
                | otherwise               = error "Not a 64-bit unsigned integer"

instance Serialize Int where
    serialize i | inDepth  8 i = serialize @Int8  $ fromIntegral i
                | inDepth 16 i = serialize @Int16 $ fromIntegral i
                | inDepth 32 i = serialize @Int32 $ fromIntegral i
                | inDepth 64 i = serialize @Int64 $ fromIntegral i
                | otherwise    = error "Not a 64-bit integer"

instance Serialize Integer where
    serialize i | inDepth 64 i = serialize @Int $ fromIntegral i
                | otherwise    = error "Not a 64-bit integer"

instance Serialize Double where
    serialize = serialize . doubleToWord

instance Serialize ByteString where
    serialize = id

instance Serialize Text where
    serialize = encodeUtf8

-- |Check that the 'Integral' value is in the n-bit bounds
inDepth :: Integral a => Int -> a -> Bool
inDepth bitDepth = let bound = 2 ^ (bitDepth - 1) :: Integer
                   in  liftA2 (&&) (>= -bound) (< bound) . fromIntegral
