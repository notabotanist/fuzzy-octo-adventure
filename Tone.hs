module Tone where

import qualified Scene
import qualified Data.Vect as Vect
import Data.Word (Word8)

type Illuminance = Float

-- |Tone Reproduction functions operate on the buffer of collected radiances
type ToneReproduce = [Scene.ColorF] -> [(Word8, Word8, Word8)]

-- |Maximum output luminance of target device
targetLdmax :: Float
targetLdmax = 300

-- |Quick and dirty approximation to pixel illuminance given R,G,B
pixIllum :: Scene.ColorF -> Illuminance
pixIllum (r, g, b) = 0.27 * r + 0.67 * g + 0.06 * b

-- |Utility function mapping functions over ColorFs
colorMap :: (Float -> Float) -> Scene.ColorF -> Scene.ColorF
colorMap f (r, g, b) = (f r, f g, f b)

-- |Scales an rgb color triple from [0,1] to [0,255]
-- If the triple value is greater than 1, clamps to 1.
colorFToWords :: Scene.ColorF -> (Word8, Word8, Word8)
colorFToWords (r, g, b) = (s r, s g, s b) where
  s = truncate.(*255).(min 1)

-- |Device model for a simple actual device with a maximum output of ldmax
-- and a gamma of 1 with standard sRGB color space.
simpleDevice :: Illuminance -> Scene.ColorF -> Scene.ColorF
simpleDevice ldmax = colorMap (/ldmax)

-- |Ward's perceptural-based tone reproduction scale factor formula.
-- A function of max display illuminance and adaptation luminance
wardScaleFactor :: Illuminance -> Illuminance -> Float
wardScaleFactor ldmax lwa = ((1.219 + (ldmax / 2)**0.4)
                            /(1.219 + lwa**0.4))**2.5

-- |Calculates the log-average of a list of numbers given a delta to prevent
-- log from going to infinity
logAvg :: Float -> [Float] -> Float
logAvg delta ls = exp ((recip sN) * (sum.map (log.(delta+)) $ ls)) where
  sN = fromIntegral $ length ls

-- |Generates a tone reproduction operator for Ward's model given a
-- target ldmax
wardTR :: Illuminance -> ToneReproduce
wardTR ldmax radiances = map ((colorFToWords).
                              (simpleDevice ldmax).
                              (colorMap (*sf)))  radiances
  where
  sf = wardScaleFactor ldmax lwa
  lwa = logAvg 0.01 (map pixIllum radiances)

-- |Generates a tone reproduction operator for Reinhard's model given
-- only a target ldmax, using the default zone (V -- a=0.18)
reinhardTR :: Illuminance -> ToneReproduce
reinhardTR = (flip reinhardTR') 0.18

-- |Generates a tone reproduction operator for Reinhard's model given
-- a target ldmax and a zone constant
reinhardTR' :: Illuminance -> Float -> ToneReproduce
reinhardTR' ldmax a radiances
  = map ((colorFToWords).(simpleDevice ldmax).(reinhard key a)) radiances
  where
  key = logAvg 0.01 (map pixIllum radiances)
  reinhard key a = colorMap ((*ldmax).filmresponse.(*(a/key)))
  filmresponse v = v / (1+v)
