data Tree a = Empty
			| Node a (Tree a) (Tree a)
			deriving (Eq, Show)

-- key (Node v l r) = v
singlon v = Node v Empty Empty

insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty k      = singlon k
insert (Node v l r) k 	| k < v     = Node v (insert l k) r
						| otherwise = Node v l (insert r k) 	 

-- fromList [x]    = singlon x
-- fromList (x:xs) = insert (fromList xs) x
fromList :: Ord b => [b] -> Tree b
fromList  = foldl insert Empty 

toList Empty = []
toList t 	 = flatten inorder t

sortList 	:: Ord a => [a] -> [a]
sortList 	 = toList . fromList

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
postorder = traverse (\n l r -> r . n . l)

flatten traversal = reverse . traversal (:) []

printTreeAscend = flatten inorder

leftorder = traverse (\n l r -> l . n)
-- leftorder min maxBound tree

