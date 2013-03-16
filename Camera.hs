module Camera
  ( Camera
  , mkCamera
  ) where

import Data.Vect ((&-),(&^),(&.),(.*.))
import qualified Data.Vect as Vect
import qualified Geom
import qualified Scene

-- |A Camera is a function which transforms a scene into camera coordinates
type Camera = Scene.ListScene -> Scene.ListScene

-- |Creates the transforming function based on position, lookat, and up
-- vectors.
mkCamera :: Geom.Point   -- ^position
         -> Vect.Normal3 -- ^lookat
         -> Vect.Normal3 -- ^up
         -> (Scene.ListScene -> Scene.ListScene)
mkCamera eyepoint lookat up = transMap
  where
    n = Vect.mkNormal (eyepoint &- (Vect.fromNormal lookat))
    u = up &^ n
    v = n &^ u
    proj = compileProj u v n eyepoint
    transMap (Scene.ListScene bg os) = Scene.ListScene bg (
      map (Scene.transform proj) os)

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
