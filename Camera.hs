module Camera
  ( Camera
  , mkCamera
  , RasterProp (RasterProp)
  , toPair
  , rays
  ) where

import Data.Vect ((&-),(&^),(&.),(.*.))
import qualified Data.Vect as Vect
import qualified Geom
import qualified Scene

-- |A Camera is a function which transforms a scene into camera coordinates
type Camera = Scene.Scene -> Scene.Scene

-- |Creates the transforming function based on position, lookat, and up
-- vectors.
mkCamera :: Geom.Point   -- ^position
         -> Geom.Point   -- ^lookat
         -> Vect.Normal3 -- ^up
         -> Camera
mkCamera eyepoint lookat up = transMap
  where
    n = Vect.mkNormal (eyepoint &- lookat)
    u = up &^ n
    v = n &^ u
    proj = compileProj u v n eyepoint
    transMap = (flip Scene.mapTransform) proj
    --transMap (Scene.ListScene bg os) = Scene.ListScene bg (
    --  map (Scene.transform proj) os)

-- |Creates the projective matrix to transform points into camera space given
-- the axes and an eyepoint
compileProj :: Vect.Normal3 -- ^u
            -> Vect.Normal3 -- ^v
            -> Vect.Normal3 -- ^n
            -> Geom.Point   -- ^eyepoint
            -> Vect.Proj4
compileProj u v n eye = linPart .*. transPart where
  linPart = Vect.linear axes
  axes = Vect.transpose (Vect.Mat3 (f u) (f v) (f n))
  f = Vect.fromNormal
  transPart = Vect.translation offset
  offset = Vect.Vec3 (eye' &. (f u)) (eye' &. (f v)) (eye' &. (f n))
  eye' = Vect.neg eye


-- Debug
{-
transCam :: Geom.Point -> Camera
transCam eyepoint = transMap where
  proj = Vect.translation (Vect.neg eyepoint)
  transMap (Scene.ListScene bg os) = Scene.ListScene bg (
    map (Scene.transform proj) os)
-}

-- |Encapsulating type for pair of integers representing the width and
-- height of a raster image
data RasterProp = RasterProp { width :: Integer, height :: Integer }
toPair (RasterProp w h) = (w, h)

-- |Generates a list of rays to trace (in camera space) given some rendering
-- properties
rays :: Float      -- ^Focal length f
     -> Float      -- ^Width of film plane
     -> Float      -- ^Height of film plane
     -> RasterProp -- ^Image width and height in pixels
     -> [Geom.Ray] -- ^List of rays to trace
rays f wP hP (RasterProp wI hI) = do
  y <- [0..(hI - 1)]
  x <- [0..(wI - 1)]
  return (originRay (Vect.Vec3 (topLeftX + (fromInteger x) * pxWidth)
                               (topLeftY - (fromInteger y) * pxHeight)
                               (-f) ))
  where
    topLeftX = (-wP) / 2 + pxWidth / 2
    topLeftY = hP / 2 - pxHeight / 2
    pxHeight = hP / (fromInteger hI)
    pxWidth = wP / (fromInteger wI)
    originRay p = Geom.Ray (Vect.Vec3 0 0 0) (Vect.mkNormal p)
