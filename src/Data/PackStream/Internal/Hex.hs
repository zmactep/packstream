{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-
 This module is copied from @hex@ module (https://github.com/taruti/haskell-hex),
 because it is not supported and was removed from stackage.
-}
module Data.PackStream.Internal.Hex (Hex(..)) where

import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L
#if !MIN_VERSION_base(4, 13, 0)
import           Control.Monad.Fail (MonadFail)
#endif

-- | Convert strings into hexadecimal and back.
class Hex t where
    -- | Convert string into hexadecimal.
    hex   :: t -> t
    -- | Convert from hexadecimal and fail on invalid input.
    unhex :: MonadFail m => t -> m t


instance Hex String where
    hex = Prelude.concatMap w
        where w ch = let s = "0123456789ABCDEF"
                         x = fromEnum ch
                     in [s !! div x 16,s !! mod x 16]
    unhex []      = return []
    unhex (a:b:r) = do x <- c a
                       y <- c b
                       (toEnum ((x * 16) + y) :) <$> unhex r
    unhex [_]      = fail "Non-even length"


c :: MonadFail m => Char -> m Int
c '0' = return 0
c '1' = return 1
c '2' = return 2
c '3' = return 3
c '4' = return 4
c '5' = return 5
c '6' = return 6
c '7' = return 7
c '8' = return 8
c '9' = return 9
c 'A' = return 10
c 'B' = return 11
c 'C' = return 12
c 'D' = return 13
c 'E' = return 14
c 'F' = return 15
c 'a' = return 10
c 'b' = return 11
c 'c' = return 12
c 'd' = return 13
c 'e' = return 14
c 'f' = return 15
c _   = fail "Invalid hex digit!"

instance Hex B.ByteString where
    hex = B.pack . hex . B.unpack
    unhex x = fmap B.pack $ unhex $ B.unpack x

instance Hex L.ByteString where
    hex = L.pack . hex . L.unpack
    unhex x = fmap L.pack $ unhex $ L.unpack x