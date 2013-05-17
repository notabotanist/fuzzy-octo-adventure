module Light4 where

import Data.Vect ((&+),(&-),(&!),(&*),(&.))
import qualified Data.Vect as Vect
import Light (Coefficients,Radiance(..))
import qualified Light
import qualified Geom4
import qualified Scene (ColorF)
import qualified Scene4
import Data.List (minimumBy)
import Data.Maybe (mapMaybe)

data Light
  = Point  -- ^Light emitting in all directions from a single 4d point
    { location :: Geom4.Point
    , color    :: Radiance
    }

-- |Container for vectors necessary during illumination
data Intersect = Intersect
  { point      :: Geom4.Point
  , normal     :: Vect.Normal4
  , incoming   :: Vect.Normal4
  }

-- |Creates Intersect object from Scene.Intersection and traced ray
mkIntersect :: Scene4.Intersection -> Geom4.Ray -> Intersect
mkIntersect (_, t, norm) r@(Geom4.Ray _ dir)
  = Intersect (Geom4.eval r t)
    norm
    dir

-- |Data container for illumination models
data IlluminationModel
  -- |Phong model illumination
  = Phong Radiance     -- ^coefficients of ambient component (ka) baked into
                       -- ambient light radiance
          Coefficients -- ^coefficients of diffuse component (kd)
          Coefficients -- ^coefficients of specular component (ks)
          Float        -- ^specular exponent (ke)
  -- |Generic texture-mapping type
  | Texture
    -- |Function encapsulating the whole texture mapping pipeline
    (Vect.Vec4 -> IlluminationModel)

-- |creates a colored plastic-like material
plasticMat :: Radiance -> Scene.ColorF -> IlluminationModel
plasticMat ambient (dr, dg, db) = Phong ka kd ks ke where
  kd = Vect.Vec3 dr dg db
  ka = ambient &! kd
  ks = Vect.Vec3 1 1 1
  ke = 16

-- |Assemble a texture pipelin
mkTexture :: Radiance -> (Vect.Vec4 -> Scene.ColorF) -> IlluminationModel
mkTexture ambient colorer
  = Texture $ (plasticMat ambient).colorer

-- |Perform illumination with the given model, calculating the radiance
-- along the returning ray (opposite the incoming ray)
illuminate :: IlluminationModel -> Intersect -> [Light] -> Radiance
illuminate (Phong kaLa kd ks ke) isect lights =
  kaLa &+ (kd &! sigmaDiffuse) &+ (ks &! sigmaSpecular) where
  sigmaDiffuse = Vect.vecSum (map diffuse lights)
  diffuse lighti = (color lighti) &* ((source lighti) &. (normal isect))
  source lighti = Vect.mkNormal ((location lighti) &- (point isect))

  sigmaSpecular = Vect.vecSum (map specular lights)
  specular lighti = (color lighti) &* (((reflect lighti) &. view) ** ke)
  view = (Vect.mkNormal).(Vect.neg).(Vect.fromNormal) $ incoming isect
  reflect lighti = Geom4.reflect' (normal isect) (source lighti)

illuminate (Texture pipeline) isect lights
  = illuminate (pipeline (point isect)) isect lights

data LitObject4 = LitObject4 IlluminationModel Scene4.Object4

tfLitObj :: Geom4.Transform -> LitObject4 -> LitObject4
tfLitObj tf (LitObject4 im obj) = LitObject4 im (Scene4.transform tf obj)

-- |Extended Intersect type to maintain illumination model
type ExIsect = (IlluminationModel, Scene4.Intersection)

-- |Extended intersect function
intersect :: Geom4.Ray -> LitObject4 -> Maybe ExIsect
intersect ray (LitObject4 im o) = fmap ((,) im) $ Scene4.intersect ray o

-- |Comparator for extended intersects
compareExIsects :: ExIsect -> ExIsect -> Ordering
compareExIsects (_, a) (_, b) = Scene4.compareIntersections a b

-- |Data structure for lit scenes
data LitScene4 = LitScene4 Radiance [LitObject4] [Light]

toColorF :: Radiance -> Scene.ColorF
toColorF (Vect.Vec3 r g b) = (r, g, b)

litTrace :: LitScene4 -> Geom4.Ray -> Radiance
litTrace (LitScene4 bg obs lis) ray = case intersections of
  [] -> bg
  ns -> shadePoint (minimumBy compareExIsects ns)
  where
  intersections = mapMaybe (intersect ray) obs
  shadePoint (im, isect) = illuminate im idata directLights where
    idata = mkIntersect isect ray
    directLights = filter visible lis
    visible light = case mapMaybe (intersect (shadow light)) obs of
      [] -> True
      ns -> let (_, (_,t,_)) = minimumBy compareExIsects ns in (dist light) < t
    shadow light = Geom4.Ray shadowPoint
                             (Vect.mkNormal ((location light) &- (point idata)))
    shadowPoint = (point idata) &+ Vect.fromNormalRadius 0.001 (normal idata)
    dist light = Vect.len ((location light) &- (point idata))
