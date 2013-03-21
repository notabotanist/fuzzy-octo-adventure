-- | Geometric definitions
module Geom
  ( Point
  , Ray(..)
  , complementBasis
  ) where

import Data.Vect (Vec3, Normal3)
import qualified Data.Vect as Vect

type Point = Vec3

data Ray = Ray { origin :: Point, direction :: Normal3 }

-- |From an input normal W, creates mutually perpendicular normals U and V
-- such that {U,V,W} is an orthonormal basis (set of axes?).
-- GeometricTools algorithm.  Voodoo, as far as I'm concerned
complementBasis :: Normal3 -> (Normal3, Normal3)
complementBasis nW
  | (abs wX) >= (abs wY) = caseA
  | otherwise            = caseB
  where
    (Vect.Vec3 wX wY wZ) = Vect.fromNormal nW
    caseA = (Vect.mkNormal uA, Vect.mkNormal vA) where
      invLength = 1 / (sqrt (wX * wX + wZ * wZ))
      uA = Vect.Vec3 uX uY uZ
      uX = (-wZ) * invLength
      uY = 0
      uZ = wX * invLength
      vA = Vect.Vec3 vX vY vZ
      vX = wY * uZ
      vY = wZ * uX - wX * uZ
      vZ = (-wY) * uX
    caseB = (Vect.mkNormal uB, Vect.mkNormal vB) where
      invLength = 1 / (sqrt (wY * wY + wZ * wZ))
      uB = Vect.Vec3 uX uY uZ
      uX = 0
      uY = wZ * invLength
      uZ = (-wY) * invLength
      vB = Vect.Vec3 vX vY vZ
      vX = wY * uZ - wZ * uY
      vY = (-wX) * uZ
      vZ = wX * uY
