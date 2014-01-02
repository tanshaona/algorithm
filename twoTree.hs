-- file: twoTree.hs
{-
data Tree maybe a = Leaf (maybe a)
                  | Node (Tree maybe a) (maybe a) (Tree maybe a) (maybe a) (Tree maybe a)
-}
-- all leafs are in the same height
data Tree t = Empty
            | Interior2 (Tree t) t (Tree t)
            | Interior3 (Tree t) t (Tree t) t (Tree t)
            | Leaf2 t
            | Leaf3 t t
            deriving Show
leaf2 a   = Interior2 Empty a Empty
leaf3 a b = Interior3 Empty a Empty b Empty

insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty k   = leaf2 k
insert (leaf2 v) k
    | k < v = leaf3 k v
    | otherwise = leaf3 v k

