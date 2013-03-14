module Scene where

--import Data.Vect () -- operators
import qualified Data.Vect as Vect
import qualified Geom
import Control.Monad (guard, mplus)

-- |Class of objects which can be intersected by rays
class Object a where
  -- |Intersects an Object with a Ray, and Maybe returns the parametric
  -- distance to the point of intersection
  intersect :: a -> Geom.Ray -> Maybe Float

-- |Sphere data structure, with center point and radius
data Sphere = Sphere { center :: Geom.Point, radius :: Float }

instance Object Sphere where
--intersect :: Sphere -> Geom.Ray -> Maybe t
  intersect (Sphere (Vect.Vec3 xc yc zc) r) (Geom.Ray origin norm)
    | discrim < 0 = Nothing
    | discrim == 0 = Just $ (-bB + discrim') / 2
    | otherwise = ming0 ((-bB + discrim') / 2) ((-bB - discrim') / 2)
    where
      (Vect.Vec3 x0 y0 z0) = origin
      (Vect.Vec3 dx dy dz) = Vect.fromNormal norm
      bB = 2 * (dx * (x0 - xc) + dy * (y0 - yc) + dz * (z0 - zc))
      bC = (x0 - xc)**2 + (y0 - yc)**2 + (z0 - zc)**2 - r**2
      discrim = bB * bB - 4 * bC
      discrim' = sqrt discrim
      ming0 a b = (guard (min a b > 0) >> return (min a b)) `mplus`
                  (guard (max a b > 0) >> return (max a b))
