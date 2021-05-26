{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B (pack, unpack, take, empty)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M (empty, fromList)

#if !MIN_VERSION_base(4, 13, 0)
import Control.Monad.Fail (MonadFail)
#endif

import Data.PackStream (PackStreamValue (..), Value(..), (=:), unpackFail)
import Data.PackStream.Internal.Hex (Hex(..))


main :: IO ()
main = hspec $ do serializeSpec
                  parseSpec

serializeSpec :: Spec
serializeSpec =
    describe "Serializer" $ do
        it "packs ()" $
            hex (pack ()) `shouldBe` "C0"
        it "packs Bool" $ do
            hex (pack False) `shouldBe` "C2"
            hex (pack True) `shouldBe` "C3"
        it "packs Int" $ do
            hex (pack (1::Int)) `shouldBe` "01"
            hex (pack (42::Int)) `shouldBe` "2A"
            hex (pack (32767::Int)) `shouldBe` "C97FFF"
            hex (pack (-32768::Int)) `shouldBe` "C98000"
            hex (pack (9223372036854775807::Int)) `shouldBe` "CB7FFFFFFFFFFFFFFF"
            hex (pack (-9223372036854775808::Int)) `shouldBe` "CB8000000000000000"
        it "packs Integer" $ do
            hex (pack (1::Integer)) `shouldBe` "01"
            hex (pack (42::Integer)) `shouldBe` "2A"
            hex (pack (1234::Integer)) `shouldBe` "C904D2"
            hex (pack (32767::Integer)) `shouldBe` "C97FFF"
            hex (pack (-32768::Integer)) `shouldBe` "C98000"
            hex (pack (-9223372036854775808::Integer)) `shouldBe` "CB8000000000000000"
        it "packs Double" $ do
            hex (pack (6.283185307179586::Double)) `shouldBe` "C1401921FB54442D18"
            hex (pack (-1.1::Double)) `shouldBe` "C1BFF199999999999A"
        it "packs ByteString" $ do
            hex (pack B.empty) `shouldBe` "CC00"
            hex (pack (B.pack [1, 2, 3])) `shouldBe` "CC03010203"
            B.take 12 (hex (pack (B.pack [1 .. 255]))) `shouldBe` "CCFF01020304"
            B.take 14 (hex (pack (B.pack [0 .. 255]))) `shouldBe` "CD010000010203"
        it "packs Text" $ do
            hex (pack (""::Text)) `shouldBe` "80"
            hex (pack ("ABCDE"::Text)) `shouldBe` "854142434445"
            hex (pack ("ABCDEFGHIJKLMNOPQRSTUVWXYZ"::Text)) `shouldBe` "D01A4142434445464748494A4B4C4D4E4F505152535455565758595A"
            hex (pack ("Größenmaßstäbe"::Text)) `shouldBe` "D0124772C3B6C39F656E6D61C39F7374C3A46265"
        it "packs []" $ do
            hex (pack ([]::[Int])) `shouldBe` "90"
            hex (pack [1::Int, 2, 3]) `shouldBe` "93010203"
            hex (pack [I 1, F (-1.1), T "", B False]) `shouldBe` "9401C1BFF199999999999A80C2"
            hex (pack [1::Int .. 32]) `shouldBe` "D4200102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F20"
            B.take 14 (hex (pack [0::Int .. 255])) `shouldBe` "D5010000010203"
        it "packs Map" $ do
            hex (pack (M.empty::Map Text Int)) `shouldBe` "A0"
            hex (pack (M.fromList ["A" =: True, "B" =: False])) `shouldBe` "A28141C38142C2"
            hex (pack (M.fromList ["A" =: (1::Int), "B" =: ("ABCDE"::Text)])) `shouldBe` "A28141018142854142434445"
            hex (pack bigMap) `shouldBe` bigMapBS

parseSpec :: Spec
parseSpec =
    describe "Parser" $ do
        it "parses ()" $ do
            v <- prepareData "C0" >>= unpackFail :: IO ()
            v `shouldBe` ()
        it "parses Bool" $ do
            t <- prepareData "C3" >>= unpackFail :: IO Bool
            t `shouldBe` True
            f <- prepareData "C2" >>= unpackFail :: IO Bool
            f `shouldBe` False
        it "parses Int" $ do
            u1 <- prepareData "01" >>= unpackFail :: IO Int
            u1 `shouldBe` 1
            u42 <- prepareData "2A" >>= unpackFail :: IO Int
            u42 `shouldBe` 42
            u32767 <- prepareData "C97FFF" >>= unpackFail :: IO Int
            u32767 `shouldBe` 32767
            umin <- prepareData "CB8000000000000000" >>= unpackFail :: IO Int
            umin `shouldBe` (-9223372036854775808)
        it "parses Integer" $ do
            u1 <- prepareData "01" >>= unpackFail :: IO Integer
            u1 `shouldBe` 1
            u42 <- prepareData "2A" >>= unpackFail :: IO Integer
            u42 `shouldBe` 42
            u1234 <- prepareData "C97FFF" >>= unpackFail :: IO Integer
            u1234 `shouldBe` 32767
            umin <- prepareData "CB8000000000000000" >>= unpackFail :: IO Integer
            umin `shouldBe` (-9223372036854775808)
        it "parses Double" $ do
            uf <- prepareData "C1BFF199999999999A" >>= unpackFail :: IO Double
            uf `shouldBe` (-1.1)
            us <- prepareData "C1401921FB54442D18" >>= unpackFail :: IO Double
            us `shouldBe` 6.283185307179586
        it "parses ByteString" $ do
            uemp <- prepareData "CC00" >>= unpackFail :: IO ByteString
            B.unpack uemp `shouldBe` []
            usmall <- prepareData "CC03010203" >>= unpackFail :: IO ByteString
            B.unpack usmall `shouldBe` [1, 2, 3]
            u255 <- unpackFail (B.pack $ [0xCC, 0xFF] ++ [1 .. 255]) :: IO ByteString
            B.unpack u255 `shouldBe` [1 .. 255]
            u256 <- unpackFail (B.pack $ [0xCD, 0x01, 0x00] ++ [0 .. 255]) :: IO ByteString
            B.unpack u256 `shouldBe` [0 .. 255]
        it "parses Text" $ do
            uemp <- prepareData "80" >>= unpackFail :: IO Text
            T.unpack uemp `shouldBe` ""
            usmall <- prepareData "854142434445" >>= unpackFail :: IO Text
            T.unpack usmall `shouldBe` "ABCDE"
            ubig <- prepareData "D01A4142434445464748494A4B4C4D4E4F505152535455565758595A" >>= unpackFail :: IO Text
            T.unpack ubig `shouldBe` "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
            uutf <- prepareData "D0124772C3B6C39F656E6D61C39F7374C3A46265" >>= unpackFail :: IO Text
            uutf `shouldBe` "Größenmaßstäbe"
        it "parses []" $ do
            uemp <- prepareData "90" >>= unpackFail :: IO [Bool]
            uemp `shouldBe` []
            usmall <- prepareData "9401C1BFF199999999999A80C2" >>= unpackFail :: IO [Value]
            usmall `shouldBe` [I 1, F (-1.1), T "", B False]
            ubig <- prepareData "D4200102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F20" >>= unpackFail :: IO [Int]
            ubig `shouldBe` [1 .. 32]
        it "parses Map" $ do
            uemp <- prepareData "A0" >>= unpackFail :: IO (Map Text Int)
            uemp `shouldBe` M.empty
            usmall <- prepareData "A28141018142854142434445" >>= unpackFail :: IO (Map Text Value)
            usmall `shouldBe` M.fromList ["A" =: (1::Int), "B" =: ("ABCDE"::Text)]
            ubig <- prepareData bigMapBS >>= unpackFail :: IO (Map Text Int)
            ubig `shouldBe` bigMap

-- Helper

prepareData :: MonadFail m => ByteString -> m ByteString
prepareData = (toStrict <$>) . unhex . fromStrict

bigMap :: Map Text Int
bigMap = M.fromList [ ("A", 1),  ("B", 2),  ("C", 3),  ("D", 4),  ("E", 5)
                    , ("F", 6),  ("G", 7),  ("H", 8),  ("I", 9),  ("J", 10)
                    , ("K", 11), ("L", 12), ("M", 13), ("N", 14), ("O", 15)
                    , ("P", 16), ("Q", 17), ("R", 18), ("S", 19), ("T", 20)
                    , ("U", 21), ("V", 22), ("W", 23), ("X", 24), ("Y", 25)
                    , ("Z", 26)]


bigMapBS :: ByteString
bigMapBS = "D81A814101814202814303814404814505814606814707814808814909814A0A814B0B814C0C814D0D814E0E814F0F815010815111815212815313815414815515815616815717815818815919815A1A"