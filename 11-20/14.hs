dupli :: [a] -> [a]
dupli x = concat [[y,y] | y <- x]