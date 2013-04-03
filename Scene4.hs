module Scene4 where

import qualified Geom4
import qualified Data.Vect as Vect
import qualified Scene (Object)

-- |Data type for hyperplanes of dimension 3
data HyperPlane = HyperPlane
  Geom4.Point  -- ^Origin
  Vect.Normal4 -- ^Basis vector i
  Vect.Normal4 -- ^Basis vector j
  Vect.Normal4 -- ^Basis vector k

-- |Calculates the normal of a HyperPlane
planeNormal :: HyperPlane -> Vect.Normal4
planeNormal (HyperPlane r i j k) = 

-- |data type of 4d objects which can be intersected by rays
data Object4
  -- |We can embed 3d objects in a 4d space by specifying a hyperplane in
  -- which it resides
  = Embed 
