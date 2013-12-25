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

flatten traversal = reverse . traversal (:) []


