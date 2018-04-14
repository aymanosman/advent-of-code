module Day3p1 where

import           Data.Function   ((&))
import           Data.List       (findIndex)
import           Data.List.Split (chunksOf)
import           Data.Maybe      (fromJust)
import           Data.Monoid     ((<>))

type Ring = [Int]
type Side = [Int]

solution :: Int -> Int
solution n =
  let
      layer = inRing n
      offset = fromJust $ findIndex (==n) (ring layer)
      adjustment = snd $ offsets layer !! offset
  in
    adjustment + layer - 1

inRing :: Int -> Int
inRing n =
  (sqrt :: Double -> Double) (fromIntegral n)
  & ceiling
  & (\a -> if even a then a + 1 else a)
  & (\m -> (m + 1) `div` 2)

ring :: Int -> Ring
ring 1 = [1..1]
ring k = let p = 2 :: Int in [kth (k-1) ^ p + 1 .. kth k ^ p]

kth :: Int -> Int
kth i = 2*i-1

offsets :: Int -> [(Int, Int)]
offsets ringNum = zip r (concatMap triangles (sides r))
  where r = ring ringNum
        sides [x] = [[x]]
        sides ns  = chunksOf (length ns`div`4) ns
        triangles [_] = [0]
        triangles ns =
          let xs = [1..length ns `div` 2]
          in tail (reverse xs) <> [0] <> xs
