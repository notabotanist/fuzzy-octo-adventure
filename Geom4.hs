-- | Geometric definitions
module Geom4
  ( Point
  , Ray(..)
  , eval
  , Transform(..)
  , tfIdentity
  , tfSeq
  ) where

import Data.Vect (Vec4, Normal4)
import qualified Data.Vect as Vect

type Point = Vec4

data Ray = Ray { origin :: Point, direction :: Normal4 }

eval :: Ray -> Float -> Point
eval (Ray origin dir) t = origin Vect.&+
                          (Vect.scalarMul t (Vect.fromNormal dir))

-- |A transformation is a rotation, followed by a translation
data Transform = Transform Vect.Mat4 Vec4

-- |Transformation equivalent to no transformation
tfIdentity :: Transform
tfIdentity = Transform Vect.idmtx Vect.zero

-- |Transformations may be combined sequentially
-- Produces a transform equivalent to performing the first, then the second
tfSeq :: Transform -> Transform -> Transform
tfSeq (Transform mR a) (Transform mS b) =
  let rot = mR Vect..*. mS
      tra = (a `Vect.rmul` mS) Vect.&+ b
  in Transform rot tra
