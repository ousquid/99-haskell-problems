import System.Random

rand_permu :: [a] -> IO [a]
rand_permu (x:[]) = return [x]
rand_permu xs = do
    r <- randomRIO (0, (length xs) - 1)
    let remaining = take r xs ++ drop (r+1) xs
    rest <- rand_permu remaining
    return ([xs !! r] ++ rest)