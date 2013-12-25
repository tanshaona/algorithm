data Tree a = Empty
			| Node a (Tree a) (Tree a)
			deriving (Eq, Show)

makeTree v = Node v Empty Empty

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

traverse step f z tree = go tree z
		where
			go Empty		z = z
			go (Node v l r) z = step (f v) (go l) (go r) z

preorder  = traverse (\n l r -> r . l . n)
inorder   = traverse (\n l r -> r . n . l)
postorder = traverse (\n l r -> r . n . l)

flatten traversal = reverse . traversal (:) []

leftorder = traverse (\n l r -> l . n)
-- leftorder min maxBound tree

