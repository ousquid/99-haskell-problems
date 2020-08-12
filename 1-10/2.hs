myButLast :: [a] -> a
myButLast [] = error "X!"
myButLast xs = head . tail . reverse $ xs