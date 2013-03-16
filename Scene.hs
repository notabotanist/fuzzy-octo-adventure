module Scene where

import Data.Vect ((&.),(&^),(&-),(&+)) -- operators (dot, cross)
import qualified Data.Vect as Vect
import qualified Geom
import Control.Monad (guard, mplus)
import Control.Applicative ((<$>))
import Data.Maybe (mapMaybe)
import Data.List (minimumBy)

-- |Represents the intersection of a ray with an object by the
-- omega distance to the intersection point and a normal at that point
type Intersection = (Object, Float, Vect.Normal3)

-- |Compare intersections by omega values
compareIntersections :: Intersection -> Intersection -> Ordering
compareIntersections (_, a, _) (_, b, _) = compare a b

-- |RGB triplet with intensity values from 0 to 1.0
type ColorF = (Float, Float, Float)

-- |data type of objects which can be intersected by rays
data Object 
  -- |Sphere data structure, with center point and radius
  = Sphere { center :: Geom.Point, radius :: Float }
  -- |Triangle data structure, with 3 points
  | Triangle Geom.Point Geom.Point Geom.Point
  deriving (Show)

-- |Colors are associated with objects at this point by type/location
objColor :: Object -> ColorF
objColor (Triangle _ _ _) = (1.0, 0, 0)
objColor (Sphere c _) = (0, 1.0, 0)

-- |Intersects an Object with a Ray, and Maybe returns the parametric
-- distance to the point of intersection
intersect :: Geom.Ray -> Object -> Maybe Intersection
-- Sphere test
intersect (Geom.Ray origin norm) s@(Sphere c@(Vect.Vec3 xc yc zc) r)
  | discrim < 0 = Nothing
--- | discrim == 0 = Just $ (-bB + discrim') / 2
  | otherwise = pair <$> ming0 ((-bB + discrim') / 2) ((-bB - discrim') / 2)
  where
    (Vect.Vec3 x0 y0 z0) = origin
    (Vect.Vec3 dx dy dz) = Vect.fromNormal norm
    bB = 2 * (dx * (x0 - xc) + dy * (y0 - yc) + dz * (z0 - zc))
    bC = (x0 - xc)**2 + (y0 - yc)**2 + (z0 - zc)**2 - r**2
    discrim = bB * bB - 4 * bC
    discrim' = sqrt discrim
    -- |returns least positive argument
    ming0 a b = (guard (min a b > 0) >> return (min a b)) `mplus`
                (guard (max a b > 0) >> return (max a b))
    pair t = (s, t, Vect.mkNormal((isect t) &- c))
    isect t = (t `Vect.scalarMul` (Vect.fromNormal norm)) &+ origin

-- Triangle test
intersect (Geom.Ray origin norm) s@(Triangle p0 p1 p2)
  | divisor == 0 = Nothing
  | t < 0 = Nothing
  | u < 0 || v < 0 || u+v > 1 = Nothing
  | otherwise = Just (s, t, norm')
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


_tfPoint :: Vect.Proj4 -> Geom.Point -> Geom.Point
_tfPoint mat = (Vect.trim).((flip Vect.rmul) mat').mk4 where
  mat' = Vect.fromProjective mat
  mk4 :: Vect.Vec3 -> Vect.Vec4
  mk4 = Vect.extendWith 1

transform :: Vect.Proj4 -> Object -> Object
transform mat (Sphere c r) = (Sphere (_tfPoint mat c) r) where
transform mat (Triangle p1 p2 p3) = (Triangle (tf p1) (tf p2) (tf p3)) where
  tf = _tfPoint mat

-- |Type class for Object containers which might be considered Scenes
class Scene a where
  -- |Trace a ray into the scene, producing some color in the image
  trace :: a -> Geom.Ray -> ColorF
  -- |Add an object to the scene
  addObject :: a -> Object -> a

-- |Implementation of Scene based on a plain list
data ListScene = ListScene ColorF [Object] deriving (Show)

instance Scene ListScene where
  trace (ListScene background os) ray = case intersections of
    [] -> background
    ns -> objColor hitObject
    where
      intersections = mapMaybe (intersect ray) os
      (hitObject, _, _) = minimumBy compareIntersections intersections

  addObject (ListScene b os) o = ListScene b (o:os)
