# packstream

[![Travis](https://img.shields.io/travis/zmactep/packstream.svg)](https://travis-ci.org/zmactep/packstream)
[![GitHub Build](https://github.com/zmactep/packstream/workflows/build/badge.svg)](https://github.com/zmactep/packstream/actions?query=workflow%3A%22build%22)
[![hackage](https://img.shields.io/hackage/v/packstream.svg)](https://hackage.haskell.org/package/packstream)
[![hackage-deps](https://img.shields.io/hackage-deps/v/packstream.svg)](https://hackage.haskell.org/package/packstream)

PackStream converter for Neo4j BOLT protocol

Documentation
-------------

To build Haddock documentation run:
```bash
$ stack haddock
```

Usage example
-------------

```haskell
ghci> :set -XOverloadedStrings
ghci> import Data.ByteString
ghci> import Data.PackStream
ghci> import Data.PackStream.Internal.Hex
ghci> hex (pack 100500)
"CA00018894"
ghci> hex (pack [True, False, True])
"93C3C2C3"
ghci> bs <- unhex "93C3C2C3" :: IO ByteString
ghci> unpack bs :: IO [Bool]
[True, False, True]
ghci> unpack bs :: IO [Value]
[B True, B False, B True]
```