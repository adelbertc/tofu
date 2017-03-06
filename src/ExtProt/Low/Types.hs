module ExtProt.Low.Types where

import Data.Word (Word64)

data Basic = Vint | Bits8 | Bits32 | Bits64Long | Bits64Float | Enum

data Composed = Tuple | Bytes | HTuple | Assoc

data Type = BasicType Basic | ComposedType Composed

newtype Tag = Tag { getTag :: Word64 }
