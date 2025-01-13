-- | Template Haskell code for shadowing 'Pack' and 'Unpack' instances
--
-- Used with "serde" quasiquoter
module M.Pack.Internal.TH
  ( borrowderivepackunpack,
    properderivepackunpack,
    borrowderivenothing,
  )
where

import Data.Functor
import Data.Serde.QQ
import Language.Haskell.TH
import M.Pack.Internal.Types

-- | shadow-derive 'Pack' and 'Unpack' instances for a type
borrowderivepackunpack :: RunUserCoercion -> Q [Dec]
borrowderivepackunpack RunUserCoercion {..} = do
  [d|
    instance Pack $datatyp where
      pack $patnormal = pack $appshadow
      {-# INLINEABLE pack #-}

    instance Unpack $datatyp where
      unpack = unpack <&> \($patshadow) -> $appnormal
      {-# INLINEABLE unpack #-}
    |]

properderivepackunpack :: Name -> Q [Dec]
properderivepackunpack n = do
  [d|
    instance Pack $(conT n)

    instance Unpack $(conT n)
    |]

-- | literally do nothing
borrowderivenothing :: RunUserCoercion -> Q [Dec]
borrowderivenothing _ = pure []
