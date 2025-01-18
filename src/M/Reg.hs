-- |
-- Module: M.Reg
-- Description: Generic dynamic registry type: {code, identifier, object}.
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- A generic registry that maintains a three-way mapping between:
--
-- * Numeric codes ('Int')
-- * Text identifiers ('Text')
-- * Objects (generic type a)
--
-- This is useful for Minecraft's registry system where objects like blocks
-- and items are identified both by numeric IDs and string identifiers.
--
-- == Usage
--
-- Create a registry:
--
-- @
-- -- imagine tr :: 'Data.Typeable.TypeRep' exists.
-- let items = [("minecraft:stone", tr @Stone), ("minecraft:dirt", tr @Dirt)]
-- case 'mkreg0' items of
--   'Just' reg -> -- Use registry
--   'Nothing' -> -- Handle duplicate keys
-- @
--
-- Look up objects:
--
-- @
-- -- By numeric code
-- case 'lkobjbycode' 0 reg of
--   'Just' obj -> -- Found
--   'Nothing' -> -- Not found
--
-- -- By string identifier
-- case 'lkcodebyid' "minecraft:stone" reg of
--   'Just' code -> -- Found
--   'Nothing' -> -- Not found
-- @
module M.Reg (Reg, mkreg0, mkreg1, lkcodebyid, lkobjbycode, lkobjbyid) where

import Data.Bifunctor
import Data.Data
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as H
import Data.IntMap.Lazy (IntMap)
import Data.IntMap.Lazy qualified as I
import Data.List (intercalate)
import Data.Text (Text)
import GHC.Generics
import Text.Printf

-- | A \"registry\" type maintaining a triple link between codes, identifiers and objects.
--
-- The registry is immutable after creation and provides efficient lookups in both directions:
--
-- * From numeric code to object (using 'IntMap')
-- * From text identifier to numeric code (using 'HashMap')
--
-- == Implementation Notes
--
-- * code -> object mapping uses lazy 'IntMap' since object types may be large
-- * identifier -> code mapping uses strict 'HashMap' for text lookups
-- * Registry cannot be modified after creation (updates planned for future)
data Reg a = Reg
  { -- | Maps numeric codes to objects. Lazy evaluation used since @a@ could be large.
    regca :: IntMap a,
    -- | Maps string identifiers to numeric codes. Strict evaluation for text lookups.
    regic :: HashMap Text Int
  }
  deriving (Eq, Ord, Generic, Typeable, Data, Functor)

-- | @[(0, blah:bloh/blee; 42), ..., (\<code\>, \<id\>; \<object\>)]@
instance (Show a) => Show (Reg a) where
  show Reg {..} =
    "["
      ++ intercalate ", " (map s (zip (I.toList regca) (H.toList regic)))
      ++ "]"
    where
      s ((c, a), (i, _)) = printf "(%d, %s; %s)" c i (show a)

-- | Create a simple registry from identifier-object pairs.
-- Numeric codes are assigned sequentially starting from 0.
--
-- Returns Nothing if there are duplicate identifiers.
--
-- == Usage
-- @
-- let reg = mkreg0 [("minecraft:stone", Stone), ("minecraft:dirt", Dirt)]
-- @
mkreg0 :: [(Text, a)] -> Maybe (Reg a)
mkreg0 = mkreg1 id
{-# INLINE mkreg0 #-}

-- | Create a registry with a transformation function applied to values.
--
-- Like 'mkreg0' but applies a function to transform the values before storing.
-- Returns Nothing if there are duplicate identifiers.
--
-- == Usage
-- @
-- let reg = mkreg1 Block [("stone", StoneData), ("dirt", DirtData)]
-- -- Creates registry with Block StoneData, Block DirtData
-- @
mkreg1 :: (b -> a) -> [(Text, b)] -> Maybe (Reg a)
mkreg1 f =
  check
    . uncurry Reg
    . bimap I.fromAscList H.fromList
    . unzip'
    . zip [0 ..]
  where
    unzip' =
      foldr
        (\(c, (i, a)) ~(p0, p1) -> ((c, f a) : p0, (i, c) : p1))
        ([], [])
    check r@Reg {..}
      | I.size regca == H.size regic = Just r
      | otherwise = Nothing -- duplicated keys

-- | Look up an object by its numeric code.
--
-- == Usage
-- @
-- case 'lkobjbycode' 0 registry of
--   Just obj -> -- Found object at code 0
--   Nothing -> -- No object at code 0
-- @
lkobjbycode :: Int -> Reg a -> Maybe a
lkobjbycode c Reg {regca} = I.lookup c regca
{-# INLINE lkobjbycode #-}

-- | Look up a numeric code by its string identifier.
--
-- == Usage
-- @
-- case 'lkcodebyid' "minecraft:stone" registry of
--   Just code -> -- Found code for identifier
--   Nothing -> -- No such identifier
-- @
lkcodebyid :: Text -> Reg a -> Maybe Int
lkcodebyid i Reg {regic} = H.lookup i regic
{-# INLINE lkcodebyid #-}

-- | Look up an object by its string identifier.
--
-- Combines 'lkcodebyid' and 'lkobjbycode' for direct identifier to object lookup.
--
-- == Usage
-- @
-- case 'lkobjbyid' "minecraft:stone" registry of
--   Just obj -> -- Found object for identifier
--   Nothing -> -- No such identifier
-- @
lkobjbyid :: Text -> Reg a -> Maybe a
lkobjbyid i r = lkcodebyid i r >>= (`lkobjbycode` r)
{-# INLINE lkobjbyid #-}
