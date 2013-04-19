module NabRay where

import Data.Vect ((&-),(&+))
import qualified Data.Vect as Vect
import qualified Scene
import qualified Geom
import qualified Camera
import qualified Light
import qualified Codec.PPM.Binary as PPM
import Data.Word (Word8)

-- |Default background color
bgColor :: Light.Radiance
bgColor = Vect.Vec3 0 0 1

-- |Ambient light
ambient :: Light.Radiance
ambient = Vect.Vec3 0.2 0.2 0.2

-- |Value transform from texture coords to red or yellow based on checker
checkerboard :: Float -> (Float, Float) -> Scene.ColorF
checkerboard checksize (u, v)
  | rowEven && colEven = red
  | rowEven && not colEven = yellow
  | not rowEven && colEven = yellow
  | not rowEven && not colEven = red
  where
    rowEven = (floor (u / checksize)) `mod` 2 == 0 
    colEven = (floor (v / checksize)) `mod` 2 == 0
    red = (1, 0, 0)
    yellow = (1, 1, 0)

-- |Planar projection function, assuming y value constant and x and z within
-- the range [-1, 1]
planar :: Vect.Vec3 -> (Float, Float)
planar (Vect.Vec3 x y z) = (u, v) where
  u = (x + 1) / 2
  v = (z + 1) / 2

-- |Creates two Triangles representing a quad
makeFloorQuad :: Geom.Point -- ^Center point
              -> Float      -- ^Half-width and half-height
              -> [Scene.Object]  -- ^Two triangles representing the quad
makeFloorQuad o e = [t1, t2] where
  t1 = Scene.Triangle a c d
  t2 = Scene.Triangle a b c
  w05 = Vect.Vec3 e 0 0
  h05 = Vect.Vec3 0 0 e
  a = o &- w05 &- h05
  b = o &- w05 &+ h05
  c = o &+ w05 &+ h05
  d = o &+ w05 &- h05

-- |Creates a scene and populates it with the assignment's objects
myScene :: Light.LitScene
myScene = Light.LitScene bgColor objects lights where
  objects = floor ++ [metal, clear]
  floor = map (Light.LitObject 0.5 0 floorTex)
              (makeFloorQuad (Vect.Vec3 7 0 (-7)) 13)
  floorTex = Light.transformTexture floorTf
               (Light.mkPhongTexture ambient id planar (checkerboard (1/13)))
  floorTf = Vect.translateAfter4 (Vect.Vec3 7 0 (-7))
          $ Vect.scalingUniformProj4 13
  metal = Light.LitObject 1 0 (plastic (0.8, 0.8, 0.8))
                          (Scene.Sphere (Vect.Vec3 4 3 (-4)) 3)
  clear = Light.LitObject 0 0 (plastic (0, 1, 0))
                          (Scene.Sphere (Vect.Vec3 0 5 0) 3)
  plastic = Light.plasticMat ambient
  lights = [(Light.Point (Vect.Vec3 (-1) 22 3) (Vect.Vec3 1 1 1))]

-- |Adds a cylinder extra to myScene
extraScene :: Light.LitScene
extraScene = Light.insertLitObj cyl myScene where
  cyl = Light.LitObject 0 0 (Light.plasticMat ambient (1, 1, 0))
    (Scene.Cylinder (Vect.Vec3 4 2 1) (Vect.mkNormal (Vect.Vec3 0 1 0)) 1 2)

-- |Adds a light extra to myScene
extraLight :: Light.LitScene
extraLight = Light.insertLight red myScene where
  red = Light.Point (Vect.Vec3 (-5) 1 0) (Vect.Vec3 1.5 0 0)

-- |Creates the camera transformation according to previously found values
myCamera :: Camera.Camera
myCamera = Camera.mkCamera (Vect.Vec3 0 5 9.4)
--          (Vect.mkNormal (Vect.Vec3 0 (-0.166769) (-0.98599604)))
                           (Vect.Vec3 0 4.833231 8.414004)
            (Vect.mkNormal (Vect.Vec3 0 1 0))
--myCamera = Camera.transCam (Vect.Vec3 0 5 9.4)

-- |The produced image properties (500,325)
myImgProp :: Camera.RasterProp
myImgProp = Camera.RasterProp 500 325

-- |Rays to be traced to form the final image.
-- They depend on some camera properties.
myRays :: [Geom.Ray]
myRays = Camera.rays 1 1.776462 1.154701 myImgProp

-- |Performs the raytrace to obtain a list of color values at each pixel
doTrace :: [Scene.ColorF]
doTrace = map (Scene.trace (myCamera (Light.toScene myScene))) myRays

-- |Tone Reproduction functions operate on the buffer of collected radiances
type ToneReproduce = [Scene.ColorF] -> [(Word8, Word8, Word8)]

-- |Scales an rgb color triple from [0,1] to [0,255]
colorFToWords :: Scene.ColorF -> (Word8, Word8, Word8)
colorFToWords (r, g, b) = (s r, s g, s b) where
  s = truncate.(*255)

-- |This one simply scales all values to 255
constTone :: ToneReproduce
constTone = map colorFToWords

-- |Scales collected irradiances linearly based on the maximum irradiance
-- values collected
linearTone :: ToneReproduce
linearTone rads = map colorFToWords scaled where
  scaled = map (scale (1/maxR) (1/maxG) (1/maxB)) rads
  scale rf gf bf (r, g, b) = (rf * r, gf * g, bf * b)
  (rs, gs, bs) = unzip3 rads
  maxR = maximum rs
  maxG = maximum gs
  maxB = maximum bs

createImage :: String -> IO ()
createImage = createImage' (Light.toScene myScene) myCamera

-- |Renders a specified scene with specified camera, then writes to file
createImage' :: Scene.Scene -> Camera.Camera -> String -> IO ()
createImage' scene cam fname = PPM.writePPM fname (Camera.toPair myImgProp) dat
  where
    dat = linearTone (map (Scene.trace (cam scene)) myRays)

-- |Performs two renders: first the standard to "test.ppm", then the extra
-- with cylinder to "cylinderTest.ppm"
main :: IO ()
main = do
  createImage "test.ppm"
  createImage' (Light.toScene extraScene) myCamera "cylinderTest.ppm"
