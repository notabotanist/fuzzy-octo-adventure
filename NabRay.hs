module NabRay where

import Data.Vect ((&-),(&+))
import qualified Data.Vect as Vect
import qualified Scene
import qualified Geom
import qualified Camera

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
myScene = buildScene (Scene.ListScene bgColor []) where
  buildScene = addFloor.addMetal.addClear
  addObject' = flip Scene.addObject
  addFloor s = foldl Scene.addObject s (makeFloorQuad (Vect.Vec3 7 0 (-7)) 13)
  addMetal = addObject' (Scene.Sphere (Vect.Vec3 4 3 (-4)) 3)
  addClear = addObject' (Scene.Sphere (Vect.Vec3 0 5 0) 3)

-- |Creates the camera transformation according to previously found values
myCamera :: Camera.Camera
myCamera = Camera.mkCamera (Vect.Vec3 0 5 9.4)
            (Vect.mkNormal (Vect.Vec3 0 (-0.166769) (-0.98599604)))
            (Vect.mkNormal (Vect.Vec3 0 1 0))
