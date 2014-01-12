module Main where

import Data.Vect ((&-),(&+))
import qualified Data.Vect as Vect
import qualified Scene
import qualified Geom
import qualified Camera
import qualified Light
import qualified Tone
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

-- |Defines a convex hull for a rupee shape in a solid-leaf BSP
rupee :: Geom.Point -> Geom.BSP
rupee c = Node (Geom.mkPlane (Vect.Vec3 0 0 (-1))
               ((Vect.Vec3 0 0 (-0.5)) Vect.&+ c) Geom.Empty Geom.Full

-- |Creates a scene and populates it with the assignment's objects
myScene :: Light.LitScene
myScene = Light.LitScene bgColor objects lights where
  objects = floor ++ [metal, clear]
  floor = map (Light.LitObject 0 0 0 floorTex)
              (makeFloorQuad (Vect.Vec3 7 0 (-7)) 13)
  floorTex = Light.transformTexture floorTf
               (Light.mkPhongTexture ambient id planar (checkerboard (1/13)))
  floorTf = Vect.translateAfter4 (Vect.Vec3 7 0 (-7))
          $ Vect.scalingUniformProj4 13
  metal = Light.LitObject 0.8 0 0 (plastic (0.2, 0.2, 0.2))
                          (Scene.Sphere (Vect.Vec3 4 3 (-4)) 3)
  clear = Light.LitObject 0 0.8 0.95 (plastic (0.2, 0.2, 0.2))
                          (Scene.Sphere (Vect.Vec3 0 5 0) 3)
  plastic = Light.plasticMat ambient
  lights = [(Light.Point (Vect.Vec3 (-1) 22 3) (Vect.Vec3 1 1 1))]

-- |Adds a cylinder extra to myScene
extraScene :: Light.LitScene
extraScene = Light.insertLitObj cyl myScene where
  cyl = Light.LitObject 0.2 0 0 (Light.plasticMat ambient (0.8, 0.8, 0))
    (Scene.Cylinder (Vect.Vec3 4 2 1) (Vect.mkNormal (Vect.Vec3 0 1 0)) 1 2)

-- |Adds a light extra to myScene
extraLight :: Light.LitScene
extraLight = Light.insertLight red myScene where
  red = Light.Point (Vect.Vec3 (-5) 1 0) (Vect.Vec3 1.5 0 0)

-- |Scales all light sources in a scene by a factor
scaleLights :: Float -> Light.LitScene -> Light.LitScene
scaleLights factor (Light.LitScene bg obs lights)
  = Light.LitScene (Vect.scalarMul factor bg) obs
                   (map (scaleLight factor) lights) where
  scaleLight f (Light.Point loc color)
    = Light.Point loc (Vect.scalarMul f color)

-- |Creates the camera transformation according to previously found values
myCamera :: Camera.Camera
myCamera = Camera.mkCamera (Vect.Vec3 0 5 9.4)
                           (Vect.Vec3 0 4.833231 8.414004)
            (Vect.mkNormal (Vect.Vec3 0 1 0))

-- |The produced image properties (500,325)
myImgProp :: Camera.RasterProp
myImgProp = Camera.RasterProp 500 325

-- |Rays to be traced to form the final image.
-- They depend on some camera properties.
myRays :: [Geom.Ray]
myRays = Camera.rays 1 1.776462 1.154701 myImgProp

-- |This one simply scales all values to 255
constTone :: Tone.ToneReproduce
constTone = map Tone.colorFToWords

-- |Scales collected irradiances linearly based on the maximum irradiance
-- values collected
linearTone :: Tone.ToneReproduce
linearTone rads = map Tone.colorFToWords scaled where
  scaled = map (scale (1/maxR) (1/maxG) (1/maxB)) rads
  scale rf gf bf (r, g, b) = (rf * r, gf * g, bf * b)
  (rs, gs, bs) = unzip3 rads
  maxR = maximum rs
  maxG = maximum gs
  maxB = maximum bs

createImage :: String -> IO ()
createImage = createImage' (Light.toScene 7 myScene) myCamera

-- |Renders a specified scene with specified camera, then writes to file
createImage' :: Scene.Scene -> Camera.Camera -> String -> IO ()
createImage' = createImageTR linearTone

-- |Renders a scene with the given tone reproduction operator
createImageTR :: Tone.ToneReproduce -> Scene.Scene -> Camera.Camera -> String
                 -> IO ()
createImageTR tr scene cam fname
  = PPM.writePPM fname (Camera.toPair myImgProp) dat
  where
    dat = tr (map (Scene.trace (cam scene)) myRays)

-- |6 renders: 3 for each tone reproduction operator, according to checkpoint7
main :: IO ()
main = do
  sequence_ [do putStrLn ("ward "++sx)
                (createImageTR (Tone.wardTR Tone.targetLdmax)
                               (Light.toScene 7 (scaleLights x myScene))
                               myCamera
                               ("ward "++sx++".ppm"))
             | x <- [1,1000,10000] :: [Float], let sx = (show.truncate) x ]
  sequence_ [do putStrLn ("reinhard "++sx)
                (createImageTR (Tone.reinhardTR Tone.targetLdmax)
                               (Light.toScene 7 (scaleLights x myScene))
                               myCamera
                               ("reinhard "++sx++".ppm"))
             | x <- [1,1000,10000] :: [Float], let sx = (show.truncate) x ]
