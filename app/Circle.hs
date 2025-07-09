module Circle where

import Common
import Control.Monad.State.Strict
import Data.Foldable (traverse_)
import Debug.Trace
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (trace)
import System.Random (Random (randomR), StdGen, getStdGen)

type Radius = Double
type Count = Int
type Circle = (P2 Double, Radius)
data PackState = PackState {gen :: StdGen, placedCircles :: [Circle]}
type PackM = State PackState

circleSpecs :: [(Radius, Count)]
circleSpecs = [(0.4, 2), (0.2, 10), (0.1, 30), (0.05, 50), (0.03, 100), (0.02, 100), (0.01, 50)]

maxPlacementTries :: Int
maxPlacementTries = 100000

maxStackSize :: Int
maxStackSize = 3

recurseRadius :: Double
recurseRadius = 0.3

rand :: (Random a) => ((a, a) -> StdGen -> (a, StdGen)) -> (a, a) -> PackM a
rand f x = do
  g <- gets gen
  let (out, g') = f x g
  modify (\s -> s{gen = g'})
  pure out

randomPoint :: PackM (P2 Double)
randomPoint = do
  theta <- rand randomR (0.0, 2 * pi)
  u <- rand randomR (0.0, 1.0)
  let r = sqrt u
  pure . p2 $ (r * cos theta, r * sin theta)

intersects :: Circle -> Circle -> Bool
intersects (c1, r1) (c2, r2) = distance c1 c2 <= r1 + r2

genCircle :: Double -> Int -> PackM (Maybe (Circle, Int))
genCircle r tries
  | tries <= 0 = pure Nothing
  | otherwise = do
      candidate <- (,) <$> randomPoint <*> pure r
      others <- gets placedCircles
      let vecToCentre = fst candidate .-. origin
          vecOut = vecToCentre ^+^ fromDirection (direction vecToCentre) ^* r
      if (candidate `intersects`) `any` others || quadrance vecOut >= 1
        then genCircle r (tries - 1)
        else pure . Just $ (candidate, tries - 1)

-- NOTE: scale & spec aren't actually used at all but we retain them
-- for potential future experimentation
circlePacking :: Double -> Int -> [(Double, Int)] -> PackM (Diagram B)
circlePacking scale stackSize specs = do
  modify (\s -> s{placedCircles = []})
  traverse_ (uncurry placeNRadii) specs
  circs <- gets placedCircles
  let (pts, rs) = unzip circs
  diagrams <- traverse renderRadius rs
  pure $
    atPoints pts diagrams
      # lw 0
      # centerXY
 where
  placeNRadii :: Double -> Int -> PackM ()
  placeNRadii r n = go n maxPlacementTries
   where
    go 0 _ = pure ()
    go _ 0 = pure ()
    go toPlace tries = do
      variance <- rand randomR (-r / 4, r / 3)
      let randomizedRadius = max (min (r + variance) 0.9) 0.01
      genCircle randomizedRadius tries >>= \case
        Nothing -> pure ()
        Just (c, tries') ->
          modify (\s -> s{placedCircles = c : placedCircles s}) *> go (toPlace - 1) tries'

  renderRadius :: Double -> PackM (Diagram B)
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
        variance <- rand randomR (-0.05, 0.05)
        let colour = normPalette $ if even stackSize then 0.25 + variance else variance
        pure $ circle r # fc colour

circleMain :: IO ()
circleMain = do
  gen <- getStdGen
  let out = evalState (circlePacking 1 0 circleSpecs) (PackState gen [])
  mainWith (out # centerXY # frame 0.1)
