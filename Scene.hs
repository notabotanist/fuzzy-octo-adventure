module Scene where

import Data.Vect ((&.),(&^),(&-),(&+)) -- operators (dot, cross)
import qualified Data.Vect as Vect
import qualified Geom
import Control.Monad (guard, mplus)
import Control.Applicative ((<$>))

-- |Represents the intersection of a ray with an object by the
-- omega distance to the intersection point and a normal at that point
type Intersection = (Float, Vect.Normal3)

-- |data type of objects which can be intersected by rays
data Object 
  -- |Sphere data structure, with center point and radius
  = Sphere { center :: Geom.Point, radius :: Float }
  -- |Triangle data structure, with 3 points
  | Triangle Geom.Point Geom.Point Geom.Point

-- |Intersects an Object with a Ray, and Maybe returns the parametric
-- distance to the point of intersection
intersect :: Object -> Geom.Ray -> Maybe Intersection
-- Sphere test
intersect (Sphere c@(Vect.Vec3 xc yc zc) r) (Geom.Ray origin norm)
  | discrim < 0 = Nothing
-- | discrim == 0 = Just $ (-bB + discrim') / 2
  | otherwise = pair <$> ming0 ((-bB + discrim') / 2) ((-bB - discrim') / 2)
  where
    (Vect.Vec3 x0 y0 z0) = origin
    (Vect.Vec3 dx dy dz) = Vect.fromNormal norm
    bB = 2 * (dx * (x0 - xc) + dy * (y0 - yc) + dz * (z0 - zc))
    bC = (x0 - xc)**2 + (y0 - yc)**2 + (z0 - zc)**2 - r**2
    discrim = bB * bB - 4 * bC
    discrim' = sqrt discrim
    ming0 a b = (guard (min a b > 0) >> return (min a b)) `mplus`
                (guard (max a b > 0) >> return (max a b))
    pair t = (t, Vect.mkNormal((isect t) &- c))
    isect t = (t `Vect.scalarMul` (Vect.fromNormal norm)) &+ origin

-- Triangle test
intersect (Triangle p0 p1 p2) (Geom.Ray origin norm)
  | divisor == 0 = Nothing
  | t < 0 = Nothing
  | u < 0 || v < 0 || u+v > 1 = Nothing
  | otherwise = Just (t, norm')
  where
    e1 = p1 &- p0
    e2 = p2 &- p0
    vT = origin &- p0
    vP = (Vect.fromNormal norm) &^ e2
    vQ = vT &^ e1
    divisor = vP &. e1
    t = (vQ &. e2) / divisor
    u = (vP &. vT) / divisor
    v = (vQ &. (Vect.fromNormal norm)) / divisor
    norm' = Vect.mkNormal(e1 &^ e2)
