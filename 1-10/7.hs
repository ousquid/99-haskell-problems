data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List xs) = foldl (\acc x -> acc ++ flatten x) [] xs
