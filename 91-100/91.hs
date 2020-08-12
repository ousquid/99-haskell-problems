import Data.List
import Data.Ord

closedKnights :: Int -> [[(Int, Int)]]
closedKnights n = filter closed $ knightsTo' n [(1,1)]
    where closed route = (head route) `elem` (candidate n $ last route)

knightsTo :: Int -> (Int, Int) -> [[(Int, Int)]]
knightsTo n dest = knightsTo' n [dest]

knightsTo' :: Int -> [(Int, Int)] -> [[(Int, Int)]]
knightsTo' n route
    | length route == n * n = [route]
    | otherwise = concat [knightsTo' n (next:route) | next <- candidate' n now, not $ next `elem` route]
        where now = head route

candidate n p = [(x, y) | m <- move, let x = fst p + fst m, let y = snd p + snd m, x > 0, y > 0, x <= n, y <= n]
    where move = [(a*x, b*y) | a <- [1,-1], b <- [1,-1], x <- [1,2], y <- [1,2], x /= y]
          
candidate' n p = sortBy cmp [(x, y) | m <- move, let x = fst p + fst m, let y = snd p + snd m, x > 0, y > 0, x <= n, y <= n]
    where move = [(a*x, b*y) | a <- [1,-1], b <- [1,-1], x <- [1,2], y <- [1,2], x /= y]
          cmp a b = comparing (length . candidate n) a b

