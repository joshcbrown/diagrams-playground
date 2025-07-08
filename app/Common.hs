module Common where

import Control.Arrow (Arrow (first))
import Control.Monad.State.Strict
import Data.Foldable (traverse_)
import Debug.Trace
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (trace)
import Diagrams.TwoD.Vector
import System.Random (Random (randomR), RandomGen, StdGen, getStdGen)

palette :: P3 Double -> P3 Double -> P3 Double -> P3 Double -> Double -> Colour Double
palette a b c d t =
  let point :: P3 Double = a + b * (fmap cos (2 * pi *^ (t *^ c + d)))
      (r, g, b') = unp3 point
   in sRGB r g b'

normPalette :: Double -> Colour Double
normPalette =
  palette
    (p3 (0.5, 0.5, 0.5))
    (p3 (0.5, 0.5, 0.5))
    (p3 (1.0, 1.0, 1.0))
    (p3 (0.0, 0.33, 0.67))
