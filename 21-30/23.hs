import System.Random

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
  gen <- getStdGen
  return [xs!!x | x <- take n (randomRs (0, (length xs)-1) gen)]
