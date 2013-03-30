module NabRay where

import Data.Vect ((&-),(&+))
import qualified Data.Vect as Vect
import qualified Scene
import qualified Geom
import qualified Camera
import qualified Codec.PPM.Binary as PPM
import Data.Word (Word8)

-- |Default background color
bgColor :: Scene.ColorF
bgColor = (0, 0, 1)

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
myScene :: Scene.ListScene
myScene = Scene.ListScene bgColor objects where
  objects = floor ++ [metal, clear]
  floor = makeFloorQuad (Vect.Vec3 7 0 (-7)) 13
  metal = Scene.Sphere (Vect.Vec3 4 3 (-4)) 3
  clear = Scene.Sphere (Vect.Vec3 0 5 0) 3

-- |Adds a cylinder extra to myScene
extraScene :: Scene.ListScene
extraScene = Scene.listScenePose cyl myScene where
  cyl = Scene.Cylinder (Vect.Vec3 4 2 1) (Vect.mkNormal (Vect.Vec3 0 1 0)) 1 2

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
doTrace = map (Scene.trace (myCamera (Scene.toScene myScene))) myRays

colorFToWords :: Scene.ColorF -> (Word8, Word8, Word8)
colorFToWords (r, g, b) = (s r, s g, s b) where
  s = truncate.(*255)

createImage :: String -> IO ()
createImage fname = PPM.writePPM fname (Camera.toPair myImgProp) (map colorFToWords doTrace)

-- |Renders a specified scene with specified camera, then writes to file
createImage' :: Scene.ListScene -> Camera.Camera -> String -> IO ()
createImage' scene cam fname = PPM.writePPM fname (Camera.toPair myImgProp) dat
  where
    dat = map colorFToWords (map (Scene.trace (cam (Scene.toScene scene))) myRays)

-- |Performs two renders: first the standard to "test.ppm", then the extra
-- with cylinder to "cylinderTest.ppm"
main :: IO ()
main = do
  createImage "test.ppm"
  createImage' extraScene myCamera "cylinderTest.ppm"
