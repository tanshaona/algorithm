-- file: lec2a.hs
-- it is recurisive,and we can modify it to tail recursion
{-
mapReduce :: (Int->Int) -> (Int->Int->Int) -> Int -> Int -> Int -> Int
mapReduce fn accu zero a b 
		| a > b = zero
		| otherwise = accu (fn a) (mapReduce fn accu zero (a+1) b)
-}

mapReduce fn accu zero a b = foldl (\a x -> accu a (fn x)) zero [a..b]
{-
mapReduce fn accu zero a b = loop a zero
	where loop itor acc 
				| itor > b  = acc
				| otherwise = loop (itor+1) (accu acc (fn itor))
-}
skipWhile :: Ord a => (a -> a -> Bool) -> [a] -> a
skipWhile f xs = skip xs
    where skip [x]      = x
          skip (x:y:ys) = if f x y then y
                          else skip (y:ys)

findFixPoint f x = skipWhile isGoodEnough $ iterate f x
    where tollerance = 0.00001
          isGoodEnough a b = abs((a-b)/a) < tollerance
{-
findFixPoint f x = 	if isGoodEnough x next 
					then next
					else findFixPoint f next
				where 	
						next 		     = f x
						tollerance       = 0.0001						
						isGoodEnough a b = abs((a-b)/a) < tollerance
-}
-- what it really matters here is that
-- the procedure averageDump take a procedure and then produces a procedure
sqrtMe x = findFixPoint (averageDump (\y -> x/y)) 1.0
		--	where averageDump f = (\z -> ((f z) + z)/2 )
        --  why I alter it from the above to the below is that
        --  it can clearly describle the really procedure that takes a procedure and returns a  procedure
            where averageDump = (\f -> (\x -> average (f x) x))
                  average x y = (x + y)/2
sqrtMe2 x = newTonMethod (\y -> x - y*y) 1.0

newTonMethod f guess = findFixPoint (\y -> y - (f y)/(df y)) guess
--    where df x = (f (x+dx) - f x) / dx 
--          dx   = 0.00001
    where df = deriv f
          deriv = (\f -> (\x -> (f (x+dx) - (f x)) / dx))
          dx = 0.00001
