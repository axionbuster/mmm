{-# OPTIONS_GHC -Wno-missing-export-lists #-}

-- |
-- Module: M.V769.I
-- Description: Internal protocol types and utilities (AI-generated)
-- Copyright: (c) axionbuster, 2025
-- License: BSD-3-Clause
--
-- Provides internal type definitions and utilities for protocol version 769,
-- including game-specific enumerations, resolution specifications, and
-- display configuration types used across different protocol states.
module M.V769.I where

import Control.DeepSeq
import Data.Bits
import Data.Data
import Data.Fixed
import Data.Hashable
import Data.Serde.QQ
import GHC.Generics
import Language.Haskell.TH.Syntax (Lift)
import M.Pack

-- | given in 1/8000 resolution
data SetEntityVelocityRes

-- | given in 1/8000 resolution
instance HasResolution SetEntityVelocityRes where
  resolution _ = 8000

data ChatMode = CMEnabled | CMCommandsOnly | CMHidden
  deriving stock (Eq, Ord, Show, Read)
  deriving stock (Enum, Bounded, Generic, Data, Typeable, Lift)
  deriving anyclass (NFData, Hashable)

data ParticleStatus = PSAll | PSDecreased | PSMinimal
  deriving stock (Eq, Ord, Show, Read)
  deriving stock (Enum, Bounded, Generic, Data, Typeable, Lift)
  deriving anyclass (NFData, Hashable)

data FilterType = PassThrough | FullyFiltered | PartiallyFiltered
  deriving stock (Eq, Ord, Show, Read)
  deriving stock (Enum, Bounded, Generic, Data, Typeable, Lift)
  deriving anyclass (NFData, Hashable)

data BossBarColor
  = BCPink
  | BCBlue
  | BCRed
  | BCGreen
  | BCYellow
  | BCPurple
  | BCWhite
  deriving stock (Eq, Ord, Show, Read)
  deriving stock (Enum, Bounded, Generic, Data, Typeable, Lift)
  deriving anyclass (NFData, Hashable)

[serde|
.derive
  Show Read Data Typeable

-- Common/Shared types first
data DisplayedSkinParts
  cape :: Bool
  jacket :: Bool
  leftsleeve :: Bool
  rightsleeve :: Bool
  leftpants :: Bool
  rightpants :: Bool
  hat :: Bool
|]

-- provided by "th-serde": Data.Serde.QQ
runusercoercion
  -- provided by M.Pack
  borrowderivepackunpack
  properderivepackunpack
  -- preparations for shadow types
  [ ''Generic,
    ''NFData,
    ''Eq,
    ''Ord
  ]

instance
  (Bits i, Integral i, Pack i, Unpack i) =>
  Bitreppable i DisplayedSkinParts
