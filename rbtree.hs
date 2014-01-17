-- file: rbtree.hs
data Color = R | B | BB -- BB : double black for deletion
        deriving (Show)

data RBTree a = Empty | BBEmpty -- doubly black empty
             -- | Node (RbTree a) a (RbTree a)
              | Node Color (RBTree a) a (RBTree a)
            deriving (Show)

-- leaf v = Node Black Empty v Empty

{-
insertT Empty v = leaf v
insertT (Node l v r) k | k < v = Node  (insertT l k) v r
                       | otherwise = Node l v (insertT r k)
-}
insertT :: Ord a => RBTree a -> a -> RBTree a
insertT t x = makeBlack $ ins t where
    ins Empty = Node R Empty x Empty
    ins (Node color a y b)
        | x <  y = balance color (ins a) y b
                    -- for a duplicated key, we handle it just by overwriting it
        | x == y = Node color a y b
        | x >  y = balance color a y (ins b)
    makeBlack (Node _ a y b) = Node B a y b

balance :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balance B (Node R (Node R a x b) y c) z d =
        Node R (Node B a x b) y (Node B c z d)
balance B (Node R a x (Node R b y c)) z d =
        Node R (Node B a x b) y (Node B c z d)
balance B a x (Node R b y (Node R c z d)) =
        Node R (Node B a x b) y (Node B c z d)
balance B a x (Node R (Node R b y c) z d) =
        Node R (Node B a x b) y (Node B c z d)
balance color l k r = Node color l k r

fromList :: Ord b0 => [b0] -> RBTree b0
fromList = foldl insertT Empty
toList   = flatten inOrder

traverse step f z t = go t z
            where
                go Empty        z = z
                go (Node _ l v r) z = step (f v) (go l) (go r) $ z

preOrder  = traverse (\n l r -> r . l . n)
inOrder   = traverse (\n l r -> r . n . l)
postOrder = traverse (\n l r -> n . r . l)

leftOrder  = traverse (\n l r -> l . n)
rightOrder = traverse (\n l r -> r . n)

flatten traversal   = reverse . traversal (:) []

minimumT = leftOrder  min (maxBound::Int)
maximumT = rightOrder max (minBound::Int)

--  X           Y
--    /    \      <---->      /    \
--   a      Y            X      c
--    /   \         /  \
--   b     c       a    b
-- rotateL :: RbTree v -> RbTree v
-- rotateL t                        = t
--rotateL (Node a x (Node b y c))   = Node (Node a x b) y c
-- rotateR  t                       = t
--rotateR (Node  (Node a x b) y c) = Node a x (Node b y c)
