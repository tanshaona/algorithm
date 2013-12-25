-- file: saddleback.hs
-- we want a function to caclulate all the possible result of the problem
-- f(x,y) = z, z >= x + y
-- invert f z = [(x,y) | x <- [0..z], y<- [0..z], f x y == z]
-- invert f z = [(x,y) | x <- [0..z], y <- [0..z-x], f x y == z]
-- because f(x,y) is increasing in the each argument,
-- so f(x,0) - f(0,0) >= x, so we get x <= z - f(0,0) 
invert1 f z = [(x,y) | x <- [0..z-(f 0 0)], y <- [0..z-x-(f 0 0)], f x y == z]

find (u,v) f z = [(x,y) | x <- [u..z], y <- [v,v-1..0], f(x,y) == z]
-- invert2 :: (a->a->a) -> a -> a
invert2 f z = find (0,z) f z 

-- in the invert2 above, we can reduce the find method, because we know the exactaly step 
-- u ascned, v descned
find2 (u,v) f z | ( u > z || v < 0 ) = []
				| (z > z')  = find2 (u+1,v) f z
				| (z < z')  = find2 (u,v-1) f z
				| (z == z') = (u,v): (find2 (u+1,v-1) f z)
					where z' = f (u,v)
 
invert3 f z = find2 (0,z) f z

