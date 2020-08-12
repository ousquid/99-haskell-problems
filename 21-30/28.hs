lsort :: [String] -> [String]
lsort [] = []
lsort (x:xs) =
    let shorterOrEqual = [a | a <- xs, length a <= length x]
        longer = [a | a <- xs, length a > length x]
    in  lsort shorterOrEqual ++ [x] ++ lsort longer

freq :: String -> [String] -> Int
freq x xs = length [a | a <- xs, length a == length x]

lfsort :: [String] -> [String]
lfsort [] = []
lfsort all@(x:xs) =
    let shorter = [a | a <- xs, freq a all < freq x all]
        longer = [a | a <- xs, freq a all > freq x all]
        equal = [a | a <- xs, freq a all == freq x all]
    in  lsort shorter ++ [x] ++ equal ++ lsort longer
