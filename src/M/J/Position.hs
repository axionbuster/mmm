-- | Support for the packed integer 'Position' format used by Java Edition
module M.J.Position
  ( Position (..),
    encodeposition,
    decodeposition,
    posapply,
    posapplyv,
  )
where

import Control.DeepSeq
import Data.Bits
import Data.Data
import Data.Hashable
import Data.Int
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import Linear
import M.Pack

-- | Deserialized position representation
newtype Position = Position {getposition :: V3 Int32}
  deriving stock (Generic, Typeable, Data, Lift)
  deriving anyclass (Hashable, NFData)
  deriving newtype (Eq, Ord, Show, Read)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}

-- | encode a 'Position' into a packed Int64
encodeposition :: Position -> Int64
encodeposition (Position (V3 x y z))
  | x > 0x1FFFFF = error "Position: X out of bounds"
  | y > 0xFFF = error "Position: Y out of bounds"
  | z > 0x1FFFFF = error "Position: Z out of bounds"
  | otherwise =
      ((fi x .&. 0x3FFFFFF) .<<. 38)
        .|. ((fi z .&. 0x3FFFFFF) .<<. 12)
        .|. (fi y .&. 0xFFF)
{-# INLINEABLE encodeposition #-}

-- | decode a packed Int64 into a 'Position'
decodeposition :: Int64 -> Position
decodeposition n = Position (V3 x y z)
  where
    x = fi $ n .>>. 38
    y = fi $ (n .<<. 52) .>>. 52
    z = fi $ (n .<<. 26) .>>. 38
{-# INLINEABLE decodeposition #-}

-- Instances for Pack and Unpack
instance Pack Position where
  pack = pack . encodeposition
  {-# INLINE pack #-}

instance Unpack Position where
  unpack = decodeposition . fi <$> unpack @Int64
  {-# INLINE unpack #-}

-- | apply a function to the x, y, and z components of a 'Position'
posapply :: (Int32 -> Int32) -> Position -> Position
posapply f (Position (V3 x y z)) = Position (V3 (f x) (f y) (f z))
{-# INLINE posapply #-}

-- | apply a function to the x, y, and z components of a 'Position'
posapplyv :: (V3 Int32 -> V3 Int32) -> Position -> Position
posapplyv f (Position v) = Position (f v)
{-# INLINE posapplyv #-}
