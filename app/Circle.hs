module Circle where

import Common
import Control.Monad.Random.Strict
import Control.Monad.State.Strict
import Data.Foldable (traverse_)
import Data.List (scanl')
import Debug.Trace
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (trace)

type Radius = Double
type Count = Int
type Circle = (P2 Double, Radius)
data PackState = PackState {placedCircles :: [Circle]}
type Pack = Rand StdGen

circleSpecs :: [(Radius, Count)]
circleSpecs = [(0.4, 2), (0.2, 10), (0.1, 30), (0.05, 50), (0.03, 100), (0.02, 100), (0.01, 50)]

maxPlacementTries :: Int
maxPlacementTries = 100000

maxStackSize :: Int
maxStackSize = 3

recurseRadius :: Double
recurseRadius = 0.3

randomPoint :: Pack (P2 Double)
randomPoint = do
  theta <- getRandomR (0.0, 2 * pi)
  u <- getRandomR (0.0, 1.0)
  let r = sqrt u
  pure . p2 $ (r * cos theta, r * sin theta)

intersects :: Circle -> Circle -> Bool
intersects (c1, r1) (c2, r2) = distance c1 c2 <= r1 + r2

-- NOTE: scale & spec aren't actually used at all but i'm keeping them around
-- for potential future experimentation
circlePacking :: Double -> Int -> [(Double, Int)] -> Pack (Diagram B)
circlePacking scale stackSize specs = do
  circs <- foldl' (uncurry . placeNRadii) (pure []) specs
  let (pts, rs) = unzip circs
  diagrams <- traverse (renderRadius) rs
  pure $
    atPoints pts diagrams
      # lw 0
      # centerXY
 where
  placeNRadii :: Pack [Circle] -> Double -> Int -> Pack [Circle]
  placeNRadii placedBefore r n = placedBefore >>= go n maxPlacementTries
   where
    go :: Int -> Int -> [Circle] -> Pack [Circle]
    go 0 _ placed = pure placed
    go _ 0 placed = pure placed
    go toPlace tries placed = do
      variance <- getRandomR (-r / 3, r / 3)
      let randomisedRadius = max (min (r + variance) 0.9) 0.01
      candidate <- (,) <$> randomPoint <*> pure randomisedRadius
      let vecToCentre = fst candidate .-. origin
          vecOut = vecToCentre ^+^ fromDirection (direction vecToCentre) ^* r
      if any (candidate `intersects`) placed || quadrance vecOut >= 1
        then go toPlace (tries - 1) placed
        else go (toPlace - 1) (tries - 1) (candidate : placed)

  renderRadius :: Double -> Pack (Diagram B)
  renderRadius r =
    if r >= recurseRadius && stackSize < maxStackSize
      then
        let
          newSpec = specs -- TODO: experiment with modifying on recursive call
          newScale = r * scale
         in
          trace (show newScale) $
            scaleUToX (2 * r) <$> circlePacking newScale (stackSize + 1) newSpec
      else do
        variance <- getRandomR (-0.05, 0.05)
        let colour = normPalette $ if even stackSize then 0.25 + variance else variance
        pure $ circle r # fc colour

circleMain :: IO ()
circleMain = do
  gen <- getStdGen
  let out =
        flip evalRand gen $
          circlePacking 1 0 circleSpecs
  mainWith (out # centerXY # frame 0.1)
