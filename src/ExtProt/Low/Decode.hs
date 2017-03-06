{-# LANGUAGE BangPatterns #-}

module ExtProt.Low.Decode where

import Data.Bits ((.&.), (.|.), Bits, clearBit, shiftL, shiftR, testBit, xor)
import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Serialize.Get (Get, getBytes, getInt64le, getWord8, getWord32le)
import Data.Serialize.IEEE754 (getFloat64le)
import Data.Word (Word8, Word32, Word64)

import ExtProt.Low.Types

fromWireType :: Word8 -> Get Type
fromWireType 0  = return $ BasicType Vint
fromWireType 2  = return $ BasicType Bits8
fromWireType 4  = return $ BasicType Bits32
fromWireType 6  = return $ BasicType Bits64Long
fromWireType 8  = return $ BasicType Bits64Float
fromWireType 10 = return $ BasicType Enum
fromWireType 1  = return $ ComposedType Tuple
fromWireType 3  = return $ ComposedType Bytes
fromWireType 5  = return $ ComposedType HTuple
fromWireType 7  = return $ ComposedType Assoc
fromWireType n  = fail   $ "Invalid wire type: " ++ show n

prefix :: Get (Tag, Type)
prefix = do
  word     <- vint
  wireType <- fromWireType $ fromIntegral $ word .&. 15 -- get last 4 bits of the word
  let tag  =  Tag $ word `shiftR` 4                     -- right shift 4 bits to get the tag
  return (tag, wireType)

bits8 :: Get Word8
bits8 = getWord8

bits32 :: Get Word32
bits32 = getWord32le

bits64Long :: Get Int64
bits64Long = getInt64le

bits64Float :: Get Double
bits64Float = getFloat64le

enum :: Get Word64
enum = fmap (getTag . fst) prefix

bytes :: Word64 -> Get ByteString
bytes = getBytes . fromIntegral

-- tuple
-- htuple
-- assoc

-- The following functions were taken from Awake Networks proto3-wire project
-- on March 5, 2017, licensed under Apache 2.0
-- https://github.com/awakenetworks/proto3-wire/blob/master/src/Proto3/Wire/Decode.hs
vint :: Get Word64
vint = loop 0 0
  where
    loop !i !w64 = do
        w8 <- getWord8
        if base128Terminal w8
            then return $ combine i w64 w8
            else loop (i + 1) (combine i w64 w8)

    base128Terminal = not . (`testBit` 7) -- check if the MSB is 1,

    combine i w64 w8 = w64 .|. (fromIntegral (w8 `clearBit` 7) `shiftL` (i * 7))

zigZagDecode :: (Num a, Bits a) => a -> a
zigZagDecode i = shiftR i 1 `xor` (-(i .&. 1))
