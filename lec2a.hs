-- file: lec2a.hs
-- it is recurisive,and we can modify it to tail recursion
{-
mapReduce :: (Int->Int) -> (Int->Int->Int) -> Int -> Int -> Int -> Int
mapReduce fn accu zero a b 
		| a > b = zero
		| otherwise = accu (fn a) (mapReduce fn accu zero (a+1) b)
-}

mapReduce fn accu zero a b = loop a zero
	where loop itor acc 
				| itor > b  = acc
				| otherwise = loop (itor+1) (accu acc (fn itor))

findFixPoint f x = 	if isGoodEnough x next 
					then next
					else findFixPoint f next
				where 	
						next 		     = f x
						tollerance       = 0.0001						
						isGoodEnough a b = abs((a-b)/a) < tollerance
-- what it really matters here is that
-- the procedure averageDump take a procedure and then produces a procedure
sqrtMe x = findFixPoint (averageDump (\y -> x/y)) 1.0
			where averageDump f = (\z -> ((f z) + z)/2 )


