module Scene where

import Data.Vect ((&.),(&^),(&-),(&+)) -- operators (dot, cross)
import qualified Data.Vect as Vect
import qualified Geom
import Control.Monad (guard, mplus)
import Control.Applicative ((<$>))
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (minimumBy, maximumBy)

-- |Represents the intersection of a ray with an object by the
-- omega distance to the intersection point and a normal at that point
type Intersection = (Object, Float, Vect.Normal3)

-- |Compare intersections by omega values
compareIntersections :: Intersection -> Intersection -> Ordering
compareIntersections (_, a, _) (_, b, _) = compare a b

-- |RGB triplet with intensity values from 0 to 1.0
type ColorF = (Float, Float, Float)

scaleColor :: ColorF -> Float -> ColorF
scaleColor (r, g, b) s = (r*s, g*s, b*s)

intersectColor :: Intersection -> ColorF
intersectColor (o, t, n) = scaleColor (objColor o) scale where
  scale = back / ((t - front) * (t - front) + back)
  back = 15
  front = 3

-- |data type of objects which can be intersected by rays
data Object 
  -- |Sphere data structure, with center point and radius
  = Sphere { center :: Geom.Point, radius :: Float }
  -- |Triangle data structure, with 3 points
  | Triangle Geom.Point Geom.Point Geom.Point
  -- |Cylinder data structure: center point, radius, height
  | Cylinder { center :: Geom.Point, axis :: Vect.Normal3,
               radius :: Float, height :: Float }
  -- |CSG object union
  | Union Object Object
  -- |CSG object intersection
  | Isect Object Object
  deriving (Show)

-- |Colors are associated with objects at this point by type/location
objColor :: Object -> ColorF
objColor (Triangle _ _ _) = (1.0, 0, 0)
objColor (Sphere c _) = (0, 1.0, 0)
objColor (Cylinder _ _ _ _) = (1.0, 1.0, 0)

-- |returns least positive argument
ming0 :: Float -> Float -> Maybe Float
ming0 a b = (guard (min a b > 0) >> return (min a b)) `mplus`
            (guard (max a b > 0) >> return (max a b))

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

-- Cylinder test: GeometricTools algorithm
-- http://www.geometrictools.com/LibMathematics/Intersection/Intersection.html
intersect (Geom.Ray rOrigin rDir) c@(Cylinder cOrigin cAxis cRadius cHeight)
  | (abs dz) == 1 = parallelCase
  | (abs dz) == 0 = perpCase
  | otherwise     = skewCase
  where
    dz = cAxis &. rDir
    (vU, vV) = Geom.complementBasis cAxis
    vW = cAxis
    diff = rOrigin &- cOrigin
    vPx = (Vect.fromNormal vU) &. diff
    vPy = (Vect.fromNormal vV) &. diff
    vPz = (Vect.fromNormal vW) &. diff
    halfHeight = cHeight / 2
    parallelCase
      | radialSqrDist < 0 = Nothing -- Line outside cylinder, no intersect
      | dz > 0    = (\t -> (c, t, cAxis')) <$>
                    ming0 ((-vPz) - halfHeight) ((-vPz) + halfHeight)
      | otherwise = (\t -> (c, t, cAxis)) <$>
                    ming0 (vPz - halfHeight) (vPz + halfHeight)
      where
        radialSqrDist = cRadius * cRadius - vPx * vPx - vPy * vPy
    cAxis' = Vect.mkNormal ((Vect.neg).(Vect.fromNormal) $ cAxis)
    vDx = vU &. rDir
    vDy = vV &. rDir
    rSqr = cRadius * cRadius
    perpCase
      | (abs vPz) > halfHeight = Nothing -- Line outside of planes of end disks
      | discr < 0 = Nothing -- Line does not intersect cylinder
      | otherwise = trip <$> ming0 (((-a1) - root) / a2) (((-a1) + root) / a2)
      where
        a0 = vPx * vPx + vPy * vPy - rSqr
        a1 = vPx * vDx + vPy * vDy
        a2 = vDx * vDx + vDy * vDy
        discr = a1 * a1 - a0 * a2
        root = sqrt discr
        trip t = (c, t, norm t)
        norm t = Vect.mkNormal((isect t) &- axisPt)
        axisPt = cOrigin &+ (vPz `Vect.scalarMul` (Vect.fromNormal cAxis))
    skewCase = leastT.(take 2).catMaybes $
      [ ((\t -> (c, t, cAxis')) <$> (testPlane (-halfHeight))),
        ((\t -> (c, t, cAxis)) <$> (testPlane halfHeight)),
        (testWall (-1)),
        (testWall 1) ]
    leastT :: [Intersection] -> Maybe Intersection
    leastT [] = Nothing
    leastT ss = Just (minimumBy compareIntersections ss)
    testPlane hh
      | (xTmp * xTmp + yTmp * yTmp) <= rSqr = Just tp
      | otherwise = Nothing
      where
        tp = (hh - vPz) / dz
        xTmp = vPx + tp * vDx
        yTmp = vPy + tp * vDy
    testWall rootFactor
      | discr < 0 = Nothing
      | (min t0 t1) <= tValue && tValue <= (max t0 t1) = Just (c, tValue, wNorm)
      | otherwise = Nothing
      where
        a0 = vPx * vPx + vPy * vPy - rSqr
        a1 = vPx * vDx + vPy * vDy
        a2 = vDx * vDx + vDy * vDy
        discr = a1 * a1 - a0 * a2
        root = sqrt discr
        tValue = ((-a1) + (rootFactor * root)) / a2
        t0 = ((-halfHeight) - vPz) / dz
        t1 = (halfHeight - vPz) / dz
        wNorm = Vect.mkNormal((isect tValue) &- axisPt)
        axisPt = cOrigin &+ (z `Vect.scalarMul` (Vect.fromNormal cAxis))
        z = tValue * dz + vPz
    isect t = rOrigin &+ (t `Vect.scalarMul` (Vect.fromNormal rDir))

-- Recurse for CSG constructs
-- For Union, choose the closer intersection
intersect ray (Union a b) = case (mapMaybe (intersect ray) [a, b]) of
  [] -> Nothing   -- Neither intersect
  ss -> Just (minimumBy compareIntersections ss)
-- For Isect, choose the further intersection iff there are 2
intersect ray (Isect a b) = case (mapMaybe (intersect ray) [a, b]) of
  [j,k] -> Just (maximumBy compareIntersections [j,k])
  _     -> Nothing


_tfPoint :: Vect.Proj4 -> Geom.Point -> Geom.Point
_tfPoint mat = (Vect.trim).((flip Vect.rmul) mat').mk4 where
  mat' = Vect.fromProjective mat
  mk4 :: Vect.Vec3 -> Vect.Vec4
  mk4 = Vect.extendWith 1

_tfNorm :: Vect.Proj4 -> Vect.Normal3 -> Vect.Normal3
_tfNorm mat = (Vect.mkNormal).(Vect.trim).((flip Vect.rmul) mat').mk4 where
  mat' = Vect.fromProjective mat
  mk4 :: Vect.Normal3 -> Vect.Vec4
  mk4 = (Vect.extendWith 0).(Vect.fromNormal)

transform :: Vect.Proj4 -> Object -> Object
transform mat (Sphere c r) = (Sphere (_tfPoint mat c) r) where
transform mat (Triangle p1 p2 p3) = (Triangle (tf p1) (tf p2) (tf p3)) where
  tf = _tfPoint mat
transform mat (Cylinder c a r h) =
  Cylinder (_tfPoint mat c) (_tfNorm mat a) r h
transform mat (Union a b) = (Union (transform mat a) (transform mat b))
transform mat (Isect a b) = (Isect (transform mat a) (transform mat b))

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
    ns -> intersectColor closest
    where
      intersections = mapMaybe (intersect ray) os
      closest = minimumBy compareIntersections intersections

  addObject (ListScene b os) o = ListScene b (o:os)
