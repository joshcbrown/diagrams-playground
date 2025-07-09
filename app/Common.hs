module Common where

import Diagrams.Prelude hiding (trace)

palette :: P3 Double -> P3 Double -> P3 Double -> P3 Double -> Double -> Colour Double
palette a b c d t =
  let point :: P3 Double = a + b * (fmap cos (2 * pi *^ (t *^ c + d)))
      (r, g, b') = unp3 point
   in sRGB r g b'

-- shoutout inigo quilez
normPalette :: Double -> Colour Double
normPalette =
  palette
    (p3 (0.5, 0.5, 0.5))
    (p3 (0.5, 0.5, 0.5))
    (p3 (1.0, 1.0, 1.0))
    (p3 (0.0, 0.33, 0.67))
