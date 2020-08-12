import Data.List (permutations, nub, sort)
import Data.List.Split (splitPlaces)

-- group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
-- (altogether 1260 solutions)

group :: [Int] -> [String] -> [[[String]]]
group xs ys = nub $ (map (sortListElems . splitPlaces xs) (permutations ys))
    where sortListElems = map sort
