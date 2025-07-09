{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Main where

import Circle
import Column
import Flower

main :: IO ()
main = columnMain -- or flowerMain
