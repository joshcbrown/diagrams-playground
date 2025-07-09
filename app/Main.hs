{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Circle
import Flower

main :: IO ()
main = circleMain -- or flowerMain
