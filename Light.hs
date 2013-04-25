module Light where

import Data.Vect ((&+),(&-),(&.),(&*),(&!))
import qualified Data.Vect as Vect
import qualified Geom
import qualified Scene
import Data.Maybe (mapMaybe)
import Data.List (minimumBy)
import Data.Tree (Tree(..), unfoldTree)
import Data.Monoid (Monoid(..))
import Data.Foldable (foldMap)

-- |Type alias for radiance in 3 frequencies: (r,g,b)
type Radiance = Vect.Vec3
-- |Type alias for coefficients
type Coefficients = Vect.Vec3

-- |Data container for light sources
data Light
  -- |Point light, emitting constant radiance in all directions
  = Point
    { location :: Geom.Point -- ^Location of point light
    , color    :: Radiance   -- ^Color of light
    }

-- |Transforms a light with an object transformer
_tfLight :: Vect.Proj4 -> Light -> Light
_tfLight mat (Point loc col) = (Point loc' col) where
  loc' = Scene._tfPoint mat loc

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
    (Vect.Vec3 -> IlluminationModel)

-- |creates a colored plastic-like material
plasticMat :: Radiance -> Scene.ColorF -> IlluminationModel
plasticMat ambient (dr, dg, db) = Phong ka kd ks ke where
  kd = Vect.Vec3 dr dg db
  ka = ambient &! kd
  ks = Vect.Vec3 1 1 1
  ke = 16

-- |Assembles a texture pipeline for coloring the diffuse component of a
-- phong model
mkPhongTexture :: Radiance
               -> (Vect.Vec3 -> Vect.Vec3)
               -> (Vect.Vec3 -> (Float, Float))
               -> ((Float, Float) -> Scene.ColorF)
               -> IlluminationModel
mkPhongTexture ambient toObjSpace projFn valTf
  = Texture $ (plasticMat ambient).valTf.projFn.toObjSpace

-- |Prepends the inverse of the specified transformation to the texture's
-- pipeline.  It nastily calculates the forward transformation matrix to then
-- be inverted.
transformTexture :: Vect.Proj4 -> IlluminationModel
  -> IlluminationModel
transformTexture mat (Texture pipe) = Texture $ pipe.invTf
  where
    invTf = Scene._tfPoint matInv
    matInv = Vect.inverse mat
-- Fallthrough case
transformTexture _ im = im

-- |Container for vectors necessary during illumination
data Intersect = Intersect
  { point      :: Geom.Point
  , normal     :: Vect.Normal3
  , incoming   :: Vect.Normal3
  , model      :: IlluminationModel
  }

-- |Creates Intersect object from Scene.Intersection and traced ray
mkIntersect :: Scene.Intersection -> Geom.Ray -> IlluminationModel -> Intersect
mkIntersect (_, t, norm) r@(Geom.Ray _ dir)
  = Intersect (Geom.eval r t)
    norm
    dir

-- |Generates a point a small distance from the intersection point along
-- the normal direction; resolves rounding errors from calculating t
epsilonPoint :: Intersect -> Geom.Point
epsilonPoint idata = (point idata) &+
  (0.001 `Vect.scalarMul` (Vect.fromNormal (normal idata)))

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
  reflect lighti = Vect.reflect' (normal isect) (source lighti)

illuminate (Texture pipeline) isect lights
  = illuminate (pipeline (point isect)) isect lights

-- |Pair between a material (IlluminationModel) and a primitive Object
data LitObject = LitObject Float Float Float IlluminationModel Scene.Object

-- |Reflectiveness constant
kr (LitObject f _ _ _ _) = f

-- |Transmissiveness constant
kt (LitObject _ f _ _ _) = f

-- |Index of refraction
eta (LitObject _ _ f _ _) = f

-- |Transforms the underlying Object of a LitObject
_tfLitObject :: Vect.Proj4 -> LitObject -> LitObject
_tfLitObject mat (LitObject kr kt n t@(Texture _) o)
  = LitObject kr kt n (transformTexture mat t) (Scene.transform mat o)
_tfLitObject mat (LitObject kr kt n im o)
  = LitObject kr kt n im (Scene.transform mat o)

-- |Extended intersection type synonym
type ExIsect = (LitObject, IlluminationModel, Scene.Intersection)

-- |Extended intersect function to maintain illumination model
intersect :: Geom.Ray -> LitObject -> Maybe ExIsect
intersect ray targ@(LitObject kr kt n im o) = case Scene.intersect ray o of
  Just i  -> Just (targ, im, i)
  Nothing -> Nothing

-- |Comparison function for extended intersections
compareExIsects :: ExIsect -> ExIsect -> Ordering
compareExIsects (_, _, a) (_, _, b) = Scene.compareIntersections a b

-- |Data container for lit scenes
data LitScene = LitScene Radiance [LitObject] [Light]

-- |Adds a LitObject to a LitScene
insertLitObj :: LitObject -> LitScene -> LitScene
insertLitObj o (LitScene bg os ls) = LitScene bg (o:os) ls

-- |Adds a Light to a LitScene
insertLight :: Light -> LitScene -> LitScene
insertLight l (LitScene bg os ls) = LitScene bg os (l:ls)

type AndPower a = (Float, a)

-- |Traces a ray into a litscene, producing intersection data and a list of
-- rays for further tracing, including reflection and transmission rays
perfectStep :: LitScene -> AndPower Geom.Ray ->
               (AndPower (Maybe Intersect), [AndPower Geom.Ray])
perfectStep (LitScene _ obs _) (k, ray) = ((k, midata), (newrays isect)) where
  isect = case anyIntersections of
    [] -> Nothing
    ns -> Just (minimumBy compareExIsects ns)
  anyIntersections = mapMaybe (intersect ray) obs
  formIdata ((LitObject _ _ _ im _), _, si) = mkIntersect si ray im
  midata = fmap formIdata isect
  newrays Nothing = []
  newrays (Just (lo, _, sceneIsect)) = reflectRays ++ transmissionRays where
    idata = mkIntersect sceneIsect ray im
    (LitObject _ _ _ im _) = lo
    reflectRays
      | (kr lo) > 0 = [ (k*(kr lo), Geom.Ray surfaceEpsilon reflectDirection) ]
      | otherwise   = []
    transmissionRays
      | (kt lo) > 0 = [ (k*(kt lo), Geom.Ray subsurfaceEpsilon transDirection)]
      | otherwise   = []
    surfaceEpsilon = (point idata) &+
      (0.001 `Vect.scalarMul` (Vect.fromNormal (normal idata)))
    subsurfaceEpsilon = (point idata) &+
      (Vect.fromNormalRadius (-0.001) transNorm)
    srcDir = (Vect.mkNormal).(Vect.neg).(Vect.fromNormal) $
             (Geom.direction ray)
    reflectDirection = Vect.reflect' (normal idata) srcDir
    transNorm
      | srcDir &. (normal idata) < 0 = Vect.flipNormal (normal idata)
      | otherwise = normal idata
    transDirection
      | srcDir &. (normal idata) < 0 =
        Vect.refract' (eta lo)
                      (Vect.flipNormal (normal idata))
                      srcDir
      | otherwise =
        Vect.refract' (1.0/(eta lo))
                      (normal idata)
                      srcDir

-- |Performs local illumination on an individual set of intersect data
localIlluminate :: LitScene -> Maybe Intersect -> Radiance
localIlluminate (LitScene bg _ _) Nothing = bg
localIlluminate (LitScene _ obs lis) (Just idata)
  = illuminate (model idata) idata directLights where
  directLights = filter visible lis
  visible light = case mapMaybe (intersect (shadow light)) obs of
    [] -> True
    ns -> (dist light) < (closestT ns)
  closestT = (\(_, _, (_,t,_)) -> t).(minimumBy compareExIsects)
  shadow light = Geom.Ray (epsilonPoint idata)
                          (Vect.mkNormal ((location light) &- (point idata)))
  dist light = Vect.len ((location light) &- (point idata))

-- |Limits a tree to a given depth.  Stops at 1.
depthLimit :: Int -> Tree a -> Tree a
depthLimit n (Node v vs)
  | n > 1     = Node v (map (depthLimit (pred n)) vs)
  | otherwise = Node v []

-- |For the tree folding magic, Vect.Vec3 must be a Monoid
instance Monoid Vect.Vec3 where
  mempty = Vect.zero
  mappend = (&+)

-- |Traces a ray into a scene, bouncing up to the given number of times.
-- Calculates the final radiance traveling along that ray to the eye
litTrace :: Int -> LitScene -> Geom.Ray -> Radiance
litTrace maxd scene ray = collapse.(depthLimit maxd).build $ (1, ray) where
  build = unfoldTree (perfectStep scene)
  collapse = foldMap powerLocal
  powerLocal (k, mi) = k `Vect.scalarMul` (localIlluminate scene mi)

-- |Maps a transformation over all objects and lights
litMapTrans :: LitScene -> Vect.Proj4 -> LitScene
litMapTrans (LitScene bg obs lis) mat
  = LitScene bg (map (_tfLitObject mat) obs) (map (_tfLight mat) lis)

toColorF (Vect.Vec3 r g b) = (r, g, b)

-- |Makes an "instance" of Scene from LitScene with a given max depth
toScene :: Int -> LitScene -> Scene.Scene
toScene d lit = Scene.Scene (toColorF.(litTrace d lit)) ((toScene d).litMapTrans lit)

