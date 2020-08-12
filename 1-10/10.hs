import Data.List
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group