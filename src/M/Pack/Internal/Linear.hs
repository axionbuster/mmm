{-# OPTIONS_GHC -Wno-orphans #-}

-- | instances for "linear" types
module M.Pack.Internal.Linear () where

import Data.Complex
import Linear
import Linear.V
import M.Pack.Internal.Newtypes
import M.Pack.Internal.Types

deriving via (PackFoldable0 V0 a) instance (Pack a) => Pack (V0 a)

deriving via (UnpackRepresentable0 V0 a) instance (Unpack a) => Unpack (V0 a)

deriving via (PackFoldable0 V1 a) instance (Pack a) => Pack (V1 a)

deriving via (UnpackRepresentable0 V1 a) instance (Unpack a) => Unpack (V1 a)

deriving via (PackFoldable0 V2 a) instance (Pack a) => Pack (V2 a)

deriving via (UnpackRepresentable0 V2 a) instance (Unpack a) => Unpack (V2 a)

deriving via (PackFoldable0 V3 a) instance (Pack a) => Pack (V3 a)

deriving via (UnpackRepresentable0 V3 a) instance (Unpack a) => Unpack (V3 a)

deriving via (PackFoldable0 V4 a) instance (Pack a) => Pack (V4 a)

deriving via (UnpackRepresentable0 V4 a) instance (Unpack a) => Unpack (V4 a)

deriving via (PackFoldable0 Complex a) instance (Pack a) => Pack (Complex a)

deriving via
  (UnpackRepresentable0 Complex a)
  instance
    (Unpack a) =>
    Unpack (Complex a)

deriving via
  (PackFoldable0 Quaternion a)
  instance
    (Pack a) =>
    Pack (Quaternion a)

deriving via
  (UnpackRepresentable0 Quaternion a)
  instance
    (Unpack a) =>
    Unpack (Quaternion a)

deriving via (PackFoldable0 (V n) a) instance (Pack a) => Pack (V n a)

deriving via
  (UnpackRepresentable0 (V n) a)
  instance
    (Unpack a, Dim n) =>
    Unpack (V n a)
