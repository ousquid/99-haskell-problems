data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)
leaf x = Branch x Empty Empty

tree65 = Branch 'n'
                (Branch 'k'
                        (Branch 'c'
                                (Branch 'a' Empty Empty)
                                (Branch 'e'
                                        (Branch 'd' Empty Empty)
                                        (Branch 'g' Empty Empty)
                                )
                        )
                        (Branch 'm' Empty Empty)
                )
                (Branch 'u'
                        (Branch 'p'
                                Empty
                                (Branch 'q' Empty Empty)
                        )
                        Empty
                )

-- copy of 65.

depth :: Tree a -> Int
depth Empty = 0
depth (Branch _ l r) = 1 + max (depth l) (depth r)

rootPos :: Tree a -> Int
rootPos t = (+1) $ sum $ drop (d - leftChildNum) $ take (d - 1) [2^n | n <- [0,1..]]
  where d = depth t
        leftChildNum = leftChildNum' t
        leftChildNum' Empty = 0
        leftChildNum' (Branch _ l _) = 1 + (leftChildNum' l)

layout :: Tree a -> Tree (a, (Int, Int))
layout t = layout' t (rootPos t) 1 (2^((depth t) - 2))

layout' :: Tree a -> Int -> Int -> Int -> Tree (a, (Int, Int))
layout' Empty pos _ _ = Empty
layout' (Branch x l r) pos depth stride = Branch (x, (pos, depth)) newLeft newRight
        where newLeft = layout' l (pos - stride) (depth+1) (stride `div` 2)
              newRight = layout' r (pos + stride) (depth+1) (stride `div` 2)

-- 66.
layout66 :: Tree (a, (Int, Int)) -> Tree (a, (Int, Int))
layout66 tree =shift_tree base_tree shift_val
    where base_tree = layout66' tree
          shift_val = - (minimum (min_list base_tree)) + 1                                                                                                  
-- root func
layout66' :: Tree (a, (Int, Int)) -> Tree (a, (Int, Int))
-- recursive layout_subtree
layout66' Empty = Empty
layout66' (Branch x l r) = layout_subtree (Branch x l_compact_tree r_compact_tree)
    where l_compact_tree = layout66' l
          r_compact_tree = layout66' r

-- subtree func
layout_subtree :: Tree (a, (Int, Int)) -> Tree (a, (Int, Int))
layout_subtree Empty = Empty 
layout_subtree tree@(Branch x l r) = 
    case (cross || edge) of True  -> tree
                            False -> layout_subtree (Branch x l_shift_tree r_shift_tree)
        where l_shift_tree = shift_tree l  1
              r_shift_tree = shift_tree r (-1)
              cross = is_cross l_shift_tree r_shift_tree
              edge = (root_edge_length tree) <= 1
          
shift_tree :: Tree (a, (Int, Int)) -> Int -> Tree (a, (Int, Int))
shift_tree Empty val = Empty
shift_tree (Branch (x, (pos, depth)) l r) val = (Branch (x, (pos + val, depth)) l_shift_tree r_shift_tree)
        where l_shift_tree = shift_tree l val
              r_shift_tree = shift_tree r val

zipWithDefault :: a -> b -> [a] -> [b] -> [(a,b)]
zipWithDefault da db la lb = let len = max (length la) (length lb)
                                 la' = la ++ (repeat da)
                                 lb' = lb ++ (repeat db)
                             in take len $ zip la' lb'  

max_list :: Tree (a, (Int, Int)) -> [Int]
max_list Empty = []
max_list (Branch (x, (pos, depth)) l r) = pos:merge_maxs
    where l_maxs = max_list l
          r_maxs = max_list r
          min_int = minBound :: Int
          merge_maxs = map (\(x, y) -> max x y) (zipWithDefault min_int min_int l_maxs r_maxs)

min_list :: Tree (a, (Int, Int)) -> [Int]
min_list Empty = []
min_list (Branch (x, (pos, depth)) l r) = pos:merge_mins
    where l_mins = min_list l
          r_mins = min_list r
          max_int = maxBound :: Int
          merge_mins = map (\(x, y) -> min x y) (zipWithDefault max_int max_int l_mins r_mins)

is_cross :: Tree (a, (Int, Int)) -> Tree (a, (Int, Int)) -> Bool
is_cross l_tree r_tree = or $ zipWith (\x y -> y<=x) l_maxs r_mins
    where l_maxs = max_list l_tree
          r_mins = min_list r_tree

root_edge_length :: Tree (a, (Int, Int)) -> Int
root_edge_length (Branch (_, (pos, _)) Empty Empty) = 0
root_edge_length (Branch (_, (pos, _)) Empty  (Branch (_, (rpos, rdepth)) _ _)) = rpos - pos
root_edge_length (Branch (_, (pos, _)) (Branch (_, (lpos, ldepth)) _ _) _) = pos - lpos