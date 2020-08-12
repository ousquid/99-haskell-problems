compress :: Eq a => [a] -> [a]
compress x = foldl compare [] x
    where compare [] x = [x]
          compare acc x = if last acc == x then acc else acc++[x]