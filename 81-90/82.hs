import Data.List

myCycle :: Eq a => a -> [(a,a)] -> [[a]]
myCycle source edges = [ 
                        [source] ++ pathList | edge <- edges, (fst edge) == source, 
                        pathList <- paths (snd edge) source [e | e <- edges, e /= edge] 
                     ]

paths :: Eq a => a -> a -> [(a,a)] -> [[a]] 
paths source sink edges 
    | source == sink = [[sink]]
    | otherwise = [
        source:path | edge<-edges, (fst edge) == source,
        path<-(paths (snd edge) sink [e|e<-edges, e/=edge])
    ]
    
cycle' :: Int -> [(Int, Int)] -> [ [Int] ]
cycle' n g = search [n] []
  where search [] result = result
        search cur result = search (go active) (arrive ++ result)
          where split = partition end cur
                end s = (last s == n) && (length s /= 1)
                active = snd split
                arrive = fst split
                go ls = [ x ++ [snd y] | x <- ls, y <- g, last x == fst y, not (snd y `elem` tail x)]