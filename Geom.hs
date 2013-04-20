-- | Geometric definitions
module Geom
  ( Point
  , Ray(..)
  , complementBasis
  , eval
  , randomCone
  ) where

import Data.Vect (Vec3, Normal3, (&.))
import qualified Data.Vect as Vect
import System.Random (RandomGen, randomR)

type Point = Vec3

data Ray = Ray { origin :: Point, direction :: Normal3 }

eval :: Ray -> Float -> Point
eval (Ray origin dir) t = origin Vect.&+
                          (Vect.scalarMul t (Vect.fromNormal dir))

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

-- |Creates a unit vector randomly distributed within a cone
randomCone :: RandomGen g => Vect.Normal3 -> Float -> g -> (Vect.Normal3, g)
randomCone coneAxis theta gen =
  let (z  ,gen1) = randomR (cos theta,1) gen
      (phi,gen2) = randomR (0,2*pi) gen1
      hypot = sqrt (1 - z * z)
      localVec = Vect.Vec3 (hypot * cos phi) (hypot * sin phi) z
  in ((Vect.mkNormal.alignPole) localVec, gen2) where
  alignPole vec
    | (Vect._2.Vect.fromNormal) coneAxis ==   1  = vec
    | (Vect._2.Vect.fromNormal) coneAxis == (-1) = Vect.neg vec
    | otherwise = Vect.rotate3 alignRot alignAxis vec where
      alignAxis = Vect.crossprod (Vect.fromNormal coneAxis) (Vect.Vec3 0 0 1)
      alignRot = acos ((Vect.fromNormal coneAxis) &. (Vect.Vec3 0 0 1))
