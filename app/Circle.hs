module Circle where

import Common
import Control.Arrow (Arrow (first))
import Control.Monad.State.Strict
import Data.Foldable (traverse_)
import Debug.Trace
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (trace)
import Diagrams.TwoD.Vector
import System.Random (Random (randomR), RandomGen, StdGen, getStdGen)

type Circle = (P2 Double, Double)
data PackState = PackState {gen :: StdGen, points :: [Circle]}

type PackM = State PackState

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

nextCirc :: Double -> Int -> PackM (Maybe (Circle, Int))
nextCirc r tries
  | tries <= 0 = pure Nothing
  | otherwise = do
      candidate <- (,) <$> randomPoint <*> pure r
      others <- gets points
      let a :: V2 Double
          a = fst candidate .-. origin
          b :: V2 Double
          b = a ^+^ fromDirection (direction a) ^* r
      if any (candidate `intersects`) others || quadrance b >= 1
        then nextCirc r (tries - 1)
        else pure . Just $ (candidate, tries - 1)

circSpec :: [(Double, Int)]
circSpec = [(0.8, 1), (0.3, 10), (0.1, 30), (0.05, 50), (0.03, 100), (0.02, 100), (0.01, 50)]

failTries :: Int
failTries = 100000

-- it would be nice to get away with not having to use scale but
-- i think we need it for palette and judgement of if we should recurse
genCircles :: Double -> Int -> [(Double, Int)] -> PackM (Diagram B)
genCircles scale count spec = do
  modify (\s -> s{points = []})
  traverse_ (uncurry tryCirc) spec
  circs <- gets points
  let (pts, rs) = unzip circs
  ds <- traverse f rs
  pure $
    ( (atPoints pts ds)
        <> circle 1 # fc outerCircColor
    )
      # lw 0
      # centerXY
 where
  tryCirc :: Double -> Int -> PackM ()
  tryCirc r n = go n failTries
   where
    go 0 _ = pure ()
    go _ 0 = pure ()
    go n tries = do
      variance <- rand randomR (-r / 3, r / 3)
      let realR = max (min (r + variance) 0.9) 0.01
      nextCirc realR tries >>= \case
        Nothing -> pure ()
        Just (c, tries') ->
          let res = modify (\s -> s{points = c : (points s)}) *> go (n - 1) tries'
           in if r == 0.45 then trace ("succeeded big " ++ show realR) res else res

  outerCircColor = if even count then normPalette 0 else normPalette 0.25

  f :: Double -> PackM (Diagram B)
  f r =
    if r >= 0.5 && count <= 20
      then
        let
          newSpec = spec
          newScale = r * scale
         in
          trace (show newScale) $ scaleUToX (2 * r) <$> genCircles newScale (count + 1) newSpec
      else do
        variance <- rand randomR (-0.05, 0.05)
        pure $ circle r # fc (normPalette (if even count then 0.25 + variance else variance))

circleMain :: IO ()
circleMain = do
  gen <- getStdGen
  let out = evalState (genCircles 1 0 circSpec) (PackState gen [])
  mainWith (out # centerXY # frame 0.1)
