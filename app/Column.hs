{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Column where

import Common
import Control.Arrow (Arrow (first))
import Control.Monad.Random
import Data.List (scanl')
import Debug.Trace
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (trace)

type Column = Rand StdGen

n :: Int
n = 200

varyAbout :: Double -> Double -> Column [Double]
varyAbout d sens = fmap (scanl' (+) d) (getRandomRs (-sens, sens))

col :: Double -> AlphaColour Double
col = dissolve 0.6 . toAlphaColour . warmPalette

diagram :: Column (Diagram B)
diagram = do
  els <- sequence (replicate 5 ellipses)
  pure $
    (hcat $ map position els)
      # frame 0.1
 where
  ys = [fromIntegral n / 10, fromIntegral n / 10 - 0.1 .. 0]
  xs = varyAbout 0 0.1
  xScales = varyAbout 2 0.1
  ellipses :: Column [(P2 Double, Diagram B)]
  ellipses = liftA2 (\xs xScales -> zipWith3 zipper xs xScales ys) xs xScales
   where
    zipper :: Double -> Double -> Double -> (P2 Double, Diagram B)
    zipper x xScale y =
      (p2 (x, y), ellipseXY xScale 1 # lw 0 # fcA (col (y / 20)))

columnMain :: IO ()
columnMain = getStdGen >>= mainWith . evalRand diagram
