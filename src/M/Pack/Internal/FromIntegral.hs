-- |
-- Module: M.Pack.Internal.FromIntegral
-- Description: Numeric conversion utilities
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Provides utility functions for numeric type conversions,
-- primarily an abbreviated form of fromIntegral.
module M.Pack.Internal.FromIntegral (fi) where

-- | an abbreviation for 'fromIntegral'
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}
