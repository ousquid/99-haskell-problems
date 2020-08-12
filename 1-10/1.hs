myLast :: [a] -> a
myLast [] = error "X!"
myLast [x] = x
myLast (x:xs) = myLast xs