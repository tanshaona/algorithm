-- file: surpassing.hs
table     :: Ord t => [t] -> [(t,Int)]
table [x]  = [(x,0)]
table zs   = join (table xs) (table ys)
		where 
				n       = length zs
				m       = n `div` 2
				(xs,ys) = splitAt m zs
join txs []  = txs
join []  tys = tys
join txs tys = [(z,c+tcount z tys) | (z,c) <- txs] ++ tys
tcount z tys = scount z (map fst tys)

-- tcount has O(n^2),so we should get a linear time solution
-- tcount z tys = length $ filter (z<) (map fst tys)
-- since filter p . map f = map f . filter (p.f)
-- so length (map fst $ filter ( (z<) . fst ) tys)
-- since length . map f = length f
-- so length $ filter ( (z<) . fst ) tys
-- since tys is sorted on the first argument
-- so length $ takeWhile ((z>=) . fst) tys
scount z ys  = length $ filter (z<) ys

msc :: Ord t => [t] -> Int
msc = maximum . map snd . table

