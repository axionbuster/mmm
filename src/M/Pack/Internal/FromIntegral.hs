-- | This module provides an abbreviation for 'fromIntegral' function
module M.Pack.Internal.FromIntegral (fi) where

-- | an abbreviation for 'fromIntegral'
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}
