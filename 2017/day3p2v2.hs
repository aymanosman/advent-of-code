{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Day3p2v2 where

import           Data.List       (nub)
import           Data.List.Split (chunksOf)
import           Data.Monoid     ((<>))

import           Test.Hspec      (hspec, it, shouldBe)

type Ring = [Int]
type Side = [Int]
type Level = Int

solution :: Int -> Int
solution n =
  head . head
  $ dropWhile null -- skip past empty rings
  $ map (dropWhile (<=n)) rings

rings :: [Ring]
rings = map (\(_,_,c) -> c) rings'

rings' = iterate up (0, mempty, [])

type WorkingOut = ([Int], [Int])

up :: (Int, WorkingOut, Ring) -> (Int, WorkingOut, Ring)
up (0, _, []) = (1, mempty, [1])
up (prev_level, _working_out,  prev_ring) =
  (,,) (prev_level + 1) (inner, outer) next
  where
    inner = contrib prev_level prev_ring
    outer = [] -- TODO
    next = reverse $ foldl (advance prev_level) [] (zip [0..] inner)

advance _  [] (_, inner) = [inner]
advance prev_level xs (i,inner) =
  (inner + head xs + cornerCase + lastTwoCase):xs
  where
    cornerCase = if i - 1 `elem` corners then xs !! 1 else 0
    lastTwoCase = if i == size - 1 || i == size - 2 then last xs else 0 -- XXX: carry last to make this case fast
    corners = take 4 $ iterate (+sideLength) (sideLength-1)
    sideLength = size `div` 4
    size = ringSize level
    level = prev_level + 1

contrib 1 _ = replicate 8 1
contrib level r =
  map (sum . nub)
  $ map (adj r)
  $ [0 .. ringSize (level + 1) - 1]

ringSize :: Level -> Int
ringSize level = (2*level-2)*4

adj :: Ring -> Int -> [Int]
adj r offset = (\n -> xs !! (n+offset)) <$> [0, 1, 2]
  where xs = 0:last streched:streched
        streched = stretch r

adjs level r = map (adj r) [0 .. ringSize (level + 1) - 1]

stretch :: Ring -> Ring
stretch [1]  = [1,1,1,1,1,1]
stretch r = concatMap extendSide $ sides r
  where extendSide xs = xs <> replicate 2 (last xs)

sides :: Ring -> [Side]
sides [x] = [[x]]
sides ns  = chunksOf (length ns `div` 4) ns


-- Tests

ring :: Level -> Ring
ring level = rings !! level

test :: IO ()
test = hspec $ do
  it "works for ring 2" $ do
    ring 2
    `shouldBe`
    [1, 2, 4, 5, 10, 11, 23, 25]

  it "works for ring 3" $ do
    ring 3
    `shouldBe`
    [26,54,57,59,122,133,142,147,304,330,351,362,747,806,880,931]


  it "works for ring 4" $ do
    take 18 (ring 4) -- couldn't be bothered calculating them all
    `shouldBe`
    [957, 1968, 2105, 2275, 2391, 2450, 5022, 5336, 5733, 6155, 6444, 6591, 13486, 14267, 15252, 16295, 17008, 17370]
