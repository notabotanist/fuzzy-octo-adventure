module NabRay4 where

import qualified Geom4
import qualified Scene4
import qualified Scene (ColorF)
import qualified Camera (RasterProp(..),toPair)
import Data.Vect.Float.Util.Dim4 (rotMatrix4, vec4X, vec4Y, vec4Z, vec4W)
import qualified Data.Vect as Vect
import qualified Codec.PPM.Binary as PPM
import Data.Word (Word8)

checkSphere :: Vect.Vec3 -> Bool
checkSphere (Vect.Vec3 x y z) = x * x + y * y + z * z < 1

myTransform :: Geom4.Transform
myTransform = Geom4.Transform rot (Vect.Vec4 0 0 (-3) 0) where
  rot = rotMatrix4 (pi / 4) (vec4X, vec4Y)
--  rot = Vect.idmtx

myScene :: Scene4.Scene
myScene = Scene4.Scene (0,0,1) [obj] where
  sphere = Scene4.Embed Scene4.basicHyperPlane checkSphere (\_ -> (1, 0, 0))
  obj = Scene4.transform myTransform sphere

-- |Renders a specified scene with specified camera, then writes to file
createImage' :: Scene4.Scene -> String -> IO ()
createImage' scene fname = PPM.writePPM fname (Camera.toPair myImgProp) dat
  where
    dat = constTone (map (Scene4.trace scene) myRays)

-- |Tone Reproduction functions operate on the buffer of collected radiances
type ToneReproduce = [Scene.ColorF] -> [(Word8, Word8, Word8)]

-- |Scales an rgb color triple from [0,1] to [0,255]
colorFToWords :: Scene.ColorF -> (Word8, Word8, Word8)
colorFToWords (r, g, b) = (s r, s g, s b) where
  s = truncate.(*255)

-- |This one simply scales all values to 255
constTone :: ToneReproduce
constTone = map colorFToWords

-- |The produced image properties (500,325)
myImgProp :: Camera.RasterProp
myImgProp = Camera.RasterProp 500 325

-- |Rays to be traced to form the final image.
-- They depend on some camera properties.
myRays :: [Geom4.Ray]
myRays = Scene4.rays 1 1.776462 1.154701 myImgProp
