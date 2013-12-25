data Tree a = Empty
			| Node a (Tree a) (Tree a)
			deriving (Eq, Show)

-- key (Node v l r) = v
leaf v = Node v Empty Empty

isEmpty Empty = True
isEmpty _	  = False

insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty k      = leaf k
insert (Node v l r) k 	| k < v     = Node v (insert l k) r
						| otherwise = Node v l (insert r k) 	 

mapT :: (t -> a) -> Tree t -> Tree a
mapT f Empty        = Empty
mapT f (Node v l r) = Node (f v) (mapT f l) (mapT f r)

-- fromList [x]    = leaf x
-- fromList (x:xs) = insert (fromList xs) x
fromList :: Ord b => [b] -> Tree b
fromList  = foldl insert Empty 

toList Empty = []
toList t 	 = flatten inorder t

sortList 	:: Ord a => [a] -> [a]
sortList 	 = toList . fromList

lookupT :: Ord a => Tree a -> a -> Tree a
lookupT Empty _ = Empty
lookupT t@(Node v l r) k | k == v = t
						| k <  v = lookupT l k
						| k >  v = lookupT r k
--maximumT :: Tree a -> Int
maximumT = rightorder max (minBound :: Int) 
minimumT = leftorder  min (maxBound :: Int)

delete Empty _ = Empty
delete (Node v l r) k 	| k < v = Node v (delete l k) r
					 	| k > v = Node v l (delete r k)
						-- x == k
						| isEmpty l = r
						| isEmpty r = l
						| otherwise = Node k' (delete l k') r
							where k' = maximumT l
					--	| otherwise = Node k' l (delete r k')
					--		where k' = minimumT r

--toList (Node v l r) = (toList l) ++ [v] ++ (toList r)
--
{-
preorderTraversal f z tree = go tree z
		where
			go Empty 		z = z
			go (Node v l r) z = let 
									z'   = f v z
									z''  = go l z'
									z''' = go r z''
								in z'''

inorderTraversal f z tree = go tree z
		where
			go Empty 		z = z
			go (Node v l r) z = let 
									z'    = go l z
									z''   = f  v z'
									z'''  = go r z''
								in z'''
inorderTraversal2 f z tree = go tree z
	where 
		go Empty		z = z
		go (Node v l r) z = go r . f v . go l $ z
-}

traverse step f z tree = go tree z
		where
			go Empty		z = z
			go (Node v l r) z = step (f v) (go l) (go r) z

preorder  = traverse (\n l r -> r . l . n)
inorder   = traverse (\n l r -> r . n . l)
postorder = traverse (\n l r -> n . r . l)

flatten traversal = reverse . traversal (:) []

printTreeAscend = flatten inorder

leftorder  = traverse (\n l r -> l . n)
rightorder = traverse (\n l r -> r . n)  
-- leftorder min maxBound tree


-- now I want to give the preorder given the inorder and postorder
rebuild [] _		= Empty
-- rebuild [p] _ 		= leaf p 
rebuild (p:ps) is 	= Node p (rebuild lp li) (rebuild rp ri)
				where 
					li = takeWhile (p /=) is
					ni = length li 
					ri = drop (ni+1) is
					(lp,rp) = splitAt ni ps
--postOrder :: [a] -> [a] -> [a]
--postOrder  = toList . rebuild

testRebuild = "\ntest rebuild tree, output the post-order result giving the in-order result and pre-order result" ++ 
			  "\n pre-order:\t" ++ show pre_order ++
 			  "\n in-order:\t" ++ show in_order ++
			  "\n post-order:\t" ++ show (flatten postorder $ rebuild pre_order in_order)
		where 
				pre_order = [1,2,4,3,5,6]
			 	in_order  = [4,2,1,5,3,6]
