{-# LANGUAGE Strict #-}

-- | Support for the packed integer 'Position' format used by Java Edition
module M.J.Position
  ( Position (..),
    encodeposition,
    parseposition,
    decodeposition,
    apply,
  )
where

import Control.DeepSeq
import Data.Bits
import Data.Data
import Data.Hashable
import Data.Int
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import M.Pack

-- | Deserialized position representation
data Position = Position
  { x :: Int32,
    y :: Int32,
    z :: Int32
  }
  deriving stock (Eq, Ord, Show, Read, Generic, Typeable, Data, Lift)
  deriving anyclass (Hashable, NFData)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- | Encode a 'Position' into a packed Int64
encodeposition :: Position -> Int64
encodeposition (Position x y z) =
  ((fi x .&. 0x3FFFFFF) .<<. 38)
    .|. ((fi z .&. 0x3FFFFFF) .<<. 12)
    .|. (fi y .&. 0xFFF)
{-# INLINE encodeposition #-}

-- | Parse a 'Position' from a ByteString
parseposition :: Parser st r Position
parseposition = decodeposition . fi <$> unpack @Int64
{-# INLINE parseposition #-}

-- | Decode a packed Int64 into a 'Position'
decodeposition :: Int64 -> Position
decodeposition n = Position x y z
  where
    x = fi $ n .>>. 38
    y = fi $ (n .<<. 52) .>>. 52
    z = fi $ (n .<<. 26) .>>. 38
{-# INLINE decodeposition #-}

-- Instances for Pack and Unpack
instance Pack Position where
  pack = pack . encodeposition
  {-# INLINE pack #-}

instance Unpack Position where
  unpack = parseposition
  {-# INLINE unpack #-}

-- | Apply a function to the x, y, and z components of a 'Position'
apply :: (Int32 -> Int32) -> Position -> Position
apply f (Position x y z) = Position (f x) (f y) (f z)
{-# INLINE apply #-}
