module Flower where

import Common
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (trace)
import Diagrams.TwoD.Vector

goldenAngle :: Radians
goldenAngle = 137.5 * (pi / 180)

type Radians = Double
type Radius = Double

phyllotaxisPoints :: Int -> Double -> [(Radius, Radians)]
phyllotaxisPoints n c = map pointFor [0 .. fromIntegral n - 1]
 where
  pointFor i = (c * sqrt i, i * goldenAngle)

flower :: [(Radius, Radians)] -> Diagram B
flower points =
  position
    ( map
        ( \(r, theta) ->
            ( origin .+^ (r *^ e (theta @@ rad))
            , circle 1 # scale (1 - 0.05 * r) # fc (normPalette (r * 0.05)) # lw 0.1
            )
        )
        points
    )

flowerMain :: IO ()
flowerMain = mainWith $ flower (phyllotaxisPoints 1000 0.5) # centerXY # frame 0.1
