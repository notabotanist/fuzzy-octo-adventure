-- | Geometric definitions
module Geom
  ( Point
  , Ray(..)
  ) where

import Data.Vect (Vec3, Normal3)

type Point = Vec3

data Ray = Ray { origin :: Point, direction :: Normal3 }
