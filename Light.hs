module Light where

import Data.Vect ((&+),(&-),(&.),(&*),(&!))
import qualified Data.Vect as Vect
import qualified Geom
import qualified Scene
import Data.Maybe (mapMaybe)
import Data.List (minimumBy)
import Data.Tree (Tree(..), unfoldTree, unfoldTreeM_BF)
import Data.Monoid (Monoid(..))
import Data.Foldable (foldMap)
import Control.Monad.State (State, state, evalState)
import Control.Monad (liftM)
import System.Random (StdGen)
import Control.Applicative ((<$>),(<*>))

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
data LitObject = LitObject Float Float IlluminationModel Scene.Object

-- |Reflectiveness constant
kr (LitObject f _ _ _) = f

-- |Transmissiveness constant
kt (LitObject _ f _ _) = f

-- |Transforms the underlying Object of a LitObject
_tfLitObject :: Vect.Proj4 -> LitObject -> LitObject
_tfLitObject mat (LitObject kr kt t@(Texture _) o)
  = LitObject kr kt (transformTexture mat t) (Scene.transform mat o)
_tfLitObject mat (LitObject kr kt im o)
  = LitObject kr kt im (Scene.transform mat o)

-- |Extended intersection type synonym
type ExIsect = (LitObject, IlluminationModel, Scene.Intersection)

-- |Extended intersect function to maintain illumination model
intersect :: Geom.Ray -> LitObject -> Maybe ExIsect
intersect ray targ@(LitObject kr kt im o) = case Scene.intersect ray o of
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
  formIdata ((LitObject _ _ im _), _, si) = mkIntersect si ray im
  midata = fmap formIdata isect
  newrays Nothing = []
  newrays (Just (lo, _, sceneIsect)) = reflectRays ++ transmissionRays where
    idata = mkIntersect sceneIsect ray im
    (LitObject _ _ im _) = lo
    reflectRays
      | (kr lo) > 0 = [ (k*(kr lo), Geom.Ray surfaceEpsilon reflectDirection) ]
      | otherwise   = []
    transmissionRays = [] -- For now
    surfaceEpsilon = (epsilonPoint idata)
    reflectDirection = Vect.reflect' (normal idata) srcDir
    srcDir = (Vect.mkNormal).(Vect.neg).(Vect.fromNormal) $
             (Geom.direction ray)

-- |Traces a ray into a litscene, producing multiple randomly sampled reflection
-- rays in a cone from the intersection point.
multiReflectStep :: Int -> Float -> LitScene -> AndPower Geom.Ray ->
  State StdGen (AndPower (Maybe Intersect), [AndPower Geom.Ray])
multiReflectStep coneRays coneSize (LitScene _ obs _) (k, ray) =
  do seeds <- newrays isect
     return ((k, midata), seeds) where
  isect = case anyIntersections of
    [] -> Nothing
    ns -> Just (minimumBy compareExIsects ns)
  anyIntersections = mapMaybe (intersect ray) obs
  formIdata ((LitObject _ _ im _), _, si) = mkIntersect si ray im
  midata = fmap formIdata isect
  newrays Nothing = return []
  newrays (Just (lo, _, sceneIsect)) = (++) <$> reflectRays <*> transmissionRays where
    idata = mkIntersect sceneIsect ray im
    (LitObject _ _ im _) = lo
    reflectRays
      | (kr lo) > 0 = mapM (reflectRay) [1..coneRays]
--      | (kr lo) > 0 = [ (k*(kr lo), Geom.Ray surfaceEpsilon reflectDirection) ]
      | otherwise   = return []
    reflectRay _ = do
      dir <- state (Geom.randomCone reflectDirection coneSize)
      return (k*(kr lo)/(fromIntegral coneRays),
              Geom.Ray surfaceEpsilon dir)
    transmissionRays = return [] -- For now
    surfaceEpsilon = epsilonPoint idata
    reflectDirection = Vect.reflect' (normal idata) srcDir
    srcDir = (Vect.mkNormal).(Vect.neg).(Vect.fromNormal) $
             (Geom.direction ray)

type Rando = State StdGen

depthlimitM :: Monad m => (b -> m (a, [b])) -> (Int, b) -> m (a, [(Int, b)])
depthlimitM step = depthstep
  where
  depthstep (nn, b)
    | nn <= 0   = do (a, bs) <- step b
                     return (a, [])
    | otherwise = do (a, bs) <- step b
                     return (a, map ((,)(nn - 1)) bs)

multiReflectStep' :: Int -> Float -> LitScene -> Rando (AndPower Geom.Ray) ->
  (Rando (AndPower (Maybe Intersect)), [Rando (AndPower Geom.Ray)])
multiReflectStep' coneRays coneSize (LitScene _ obs _) seed = undefined
-- This doesn't make any sense.

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

-- |Traces a ray into a scene, bouncing a given number of times.
-- Reflections produce a number of rays within a cone of given angle width.
-- They are distributed randomly according to the provided RandomGen
litTraceCone :: Int -> Int -> Float -> StdGen -> LitScene -> Geom.Ray -> Radiance
litTraceCone maxd coneRays coneAngle gen scene ray =
  collapse.build $ (maxd, (1, ray)) where
  build = ((flip evalState) gen).
    (unfoldTreeM_BF.depthlimitM) (multiReflectStep coneRays coneAngle scene)
  collapse = foldMap powerLocal
  powerLocal (k, mi) = k `Vect.scalarMul` (localIlluminate scene mi)

toColorF (Vect.Vec3 r g b) = (r, g, b)

-- |Maps a transformation over all objects and lights
litMapTrans :: LitScene -> Vect.Proj4 -> LitScene
litMapTrans (LitScene bg obs lis) mat
  = LitScene bg (map (_tfLitObject mat) obs) (map (_tfLight mat) lis)

-- |Makes an "instance" of Scene from LitScene
toScene :: LitScene -> Scene.Scene
toScene lit = Scene.Scene (toColorF.litTrace 5 lit) (toScene.litMapTrans lit)

-- |Makes an "instance" of Scene from LitScene using multiple reflection rays
multiReflectScene :: Int -> Float -> StdGen -> LitScene -> Scene.Scene
multiReflectScene coneRays coneAngle gen lit =
  Scene.Scene (toColorF.litTraceCone 3 coneRays coneAngle gen lit)
              (multiReflectScene coneRays coneAngle gen.litMapTrans lit)
