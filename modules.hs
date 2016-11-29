import Data.List
import qualified Data.Map as Map
import Geometry

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

search' :: (Eq a) => [a] -> [a] -> Bool
search' needle haystack = 
  let nlen = length needle
  in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key xs = foldl (\acc (k,v) -> if key == k then Just v else acc) Nothing xs

testMap = Map.fromList [("betty", "1231"), ("bonnie", "6787")]
