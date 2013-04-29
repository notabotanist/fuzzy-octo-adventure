module Scene4 where

import qualified Geom4
import Data.Vect ((&.),(&-),(&+))
import qualified Data.Vect as Vect
import Scene (ColorF)
import Camera (RasterProp(..))
import Data.Maybe (mapMaybe)

-- |Data type for hyperplanes of dimension 3
data HyperPlane = HyperPlane
  Geom4.Point  -- ^Origin
  Vect.Normal4 -- ^Basis vector i
  Vect.Normal4 -- ^Basis vector j
  Vect.Normal4 -- ^Basis vector k
  Vect.Normal4 -- ^Normal to the hyperplane

-- |Calculates the normal of a HyperPlane
planeNormal :: HyperPlane -> Vect.Normal4
planeNormal (HyperPlane r i j k norm) = norm

-- |Basic hyperplane which lives at the origin and is perpendicular to w
basicHyperPlane :: HyperPlane
basicHyperPlane = HyperPlane (Vect.Vec4 0 0 0 0)
                             (Vect.mkNormal (Vect.Vec4 1 0 0 0))
                             (Vect.mkNormal (Vect.Vec4 0 1 0 0))
                             (Vect.mkNormal (Vect.Vec4 0 0 1 0))
                             (Vect.mkNormal (Vect.Vec4 0 0 0 1))

-- |Project a 4d point into the hyperplane
project :: HyperPlane -> Vect.Vec4 -> Vect.Vec3
project (HyperPlane r i j k vN) p = Vect.Vec3 px py pz where
  p' = p &- r
  px = p' &. Vect.fromNormal i
  py = p' &. Vect.fromNormal j
  pz = p' &. Vect.fromNormal k

-- |data type of 4d objects which can be intersected by rays
data Object4
  -- |We can embed 3d objects in a 4d space by specifying a hyperplane in
  -- which it resides, and a function which indicates if a 3d point lies inside
  -- the object
  = Embed
    HyperPlane            -- ^ Containing hyperplane
    (Vect.Vec3 -> Bool)   -- ^ Intersection function
    (Vect.Vec3 -> ColorF) -- ^ Shading function

-- |shades an object based on the intersection point
objColor :: Object4 -> Vect.Vec4 -> ColorF
objColor (Embed hp _ f) = f.project hp

type Intersection = (Object4, Float)

intersect :: Geom4.Ray -> Object4 -> Maybe Intersection
-- Embedded 3d object case
intersect (Geom4.Ray p0 v) o@(Embed hp isContained _)
  | div == 0       = Nothing
  | t < 0          = Nothing
  | isContained p' = Just (o, t)
  | otherwise      = Nothing
  where
  (HyperPlane r i j k vN) = hp
  div = vN &. v
  t = -((Vect.fromNormal vN &. p0) - sE) / div
  sE = Vect.fromNormal vN &. r
  p' = Vect.Vec3 px py pz
  preIsect = (p0 &- r &+ (t `Vect.scalarMul` Vect.fromNormal v))
  px = preIsect &. Vect.fromNormal i
  py = preIsect &. Vect.fromNormal j
  pz = preIsect &. Vect.fromNormal k

transform :: Geom4.Transform -> Object4 -> Object4
transform (Geom4.Transform rot trans) (Embed hp f g) = Embed hp' f g where
  (HyperPlane r i j k vN) = hp
  hp' = HyperPlane r' i' j' k' vN'
  r' = (Vect.rmul r rot) &+ trans
  axisRot = (Vect.mkNormal).((flip Vect.rmul) rot).(Vect.fromNormal)
  i' = axisRot i
  j' = axisRot j
  k' = axisRot k
  vN' = axisRot vN

data Scene = Scene ColorF [Object4]

trace :: Scene -> Geom4.Ray -> ColorF
trace (Scene bg os) ray = case intersections of
  [] -> bg
  ns -> let (o, t) = head ns in objColor o (Geom4.eval ray t)
  where
  intersections = mapMaybe (intersect ray) os

-- |Generates a list of rays to trace (in camera space) given some rendering
-- properties
rays :: Float      -- ^Focal length f
     -> Float      -- ^Width of film plane
     -> Float      -- ^Height of film plane
     -> RasterProp -- ^Image width and height in pixels
     -> [Geom4.Ray] -- ^List of rays to trace
rays f wP hP (RasterProp wI hI) = do
  y <- [0..(hI - 1)]
  x <- [0..(wI - 1)]
  return (originRay (Vect.Vec4 (topLeftX + (fromInteger x) * pxWidth)
                               (topLeftY - (fromInteger y) * pxHeight)
                               (-f) 0))
  where
    topLeftX = (-wP) / 2 + pxWidth / 2
    topLeftY = hP / 2 - pxHeight / 2
    pxHeight = hP / (fromInteger hI)
    pxWidth = wP / (fromInteger wI)
    originRay p = Geom4.Ray (Vect.Vec4 0 0 0 0) (Vect.mkNormal p)