module NabRay4 where

import qualified Geom4
import qualified Scene4
import qualified Light (Radiance, plasticMat)
import qualified Light4
import qualified Scene (ColorF)
import qualified Camera (RasterProp(..),toPair)
import Data.Vect.Float.Util.Dim4 (rotMatrix4, vec4X, vec4Y, vec4Z, vec4W)
import qualified Data.Vect as Vect
import qualified Codec.PPM.Binary as PPM
import Data.Word (Word8)

checkSphere :: Vect.Vec3 -> Bool
checkSphere (Vect.Vec3 x y z) = x * x + y * y + z * z < 1

colorSphere :: Vect.Vec3 -> Scene.ColorF
colorSphere (Vect.Vec3 x y z) = (abs x, abs y, abs z)

-- |Default background color
bgColor :: Light.Radiance
bgColor = Vect.Vec3 0 0 1

-- |Ambient light
ambient :: Light.Radiance
ambient = Vect.Vec3 0.2 0.2 0.2

makeTransform :: Float -> Geom4.Transform
makeTransform phi = Geom4.tfSeq tR tS where
  tS = Geom4.Transform rotS (Vect.Vec4 0 0 (-3) 0.25)
  rotS = rotMatrix4 (pi / 4) (vec4X, vec4Y)
  tR = Geom4.Transform (rotMatrix4 phi (vec4X, vec4W)) Vect.zero

myTransform :: Geom4.Transform
myTransform = makeTransform (pi / 4)

myLight :: Light4.Light
myLight = Light4.Point (Vect.Vec4 0 3 0 0) (Vect.Vec3 1 1 1)

makeScene :: Float -> Light4.LitScene4
makeScene phi = Light4.LitScene4 (Vect.Vec3 1 1 1) [lobj] [myLight] where
  sphere = Scene4.Embed Scene4.basicHyperPlane checkSphere colorSphere
  obj = Scene4.transform (makeTransform phi) sphere
  lobj = Light4.LitObject4 (Light.plasticMat ambient (0, 0, 1)) obj

myScene :: Scene4.Scene
myScene = Scene4.Scene (1,1,1) [obj] where
  sphere = Scene4.Embed Scene4.basicHyperPlane checkSphere colorSphere
  obj = Scene4.transform myTransform sphere

hyperScene :: Float -> Light4.LitScene4
hyperScene w = Light4.LitScene4 (Vect.Vec3 0 0 1) [lobj] [myLight] where
  obj = sphere
  sphere = Scene4.HyperSphere (Vect.Vec4 0 0 (-3) w) 1
  lobj = Light4.LitObject4 (Light.plasticMat ambient (0, 0.8, 0)) obj

-- |Renders a specified scene with specified camera, then writes to file
createImage' :: Scene4.Scene -> String -> IO ()
createImage' scene fname = PPM.writePPM fname (Camera.toPair myImgProp) dat
  where
    dat = constTone (map (Scene4.trace scene) myRays)

createLitImage :: Light4.LitScene4 -> String -> IO ()
createLitImage scene fname = PPM.writePPM fname (Camera.toPair myImgProp) dat
  where
  dat = linearTone (map (Light4.toColorF.Light4.litTrace scene) myRays)

-- |Renders the scene using the given angle to construct its transformation
createImageAngle :: Float -> IO ()
createImageAngle phi = createLitImage scene name where
  scene = makeScene phi
  name = "4d-" ++ (take 4 $ show phi) ++ ".ppm"

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

-- |The produced image properties (500,325)
myImgProp :: Camera.RasterProp
myImgProp = Camera.RasterProp 500 325

-- |Rays to be traced to form the final image.
-- They depend on some camera properties.
myRays :: [Geom4.Ray]
myRays = Scene4.rays 1 1.776462 1.154701 myImgProp
