-- | Geometric definitions
module Geom4
  ( Point
  , Ray(..)
  , eval
  , Transform(..)
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