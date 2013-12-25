-- infixr \\ 3
import Data.Array
import Data.List

-- vs \\ xs = filter (`notElem` xs) vs

-- minFree xs = head $ [0..] \\ xs

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist    :: [Int] -> Array Int Bool
checklist xs =  accumArray (||) False (0,n)
				(zip (filter (<=n) xs) (repeat True))
			 	where n = length xs 
minFree = search . checklist

{-
[0..] \\ xs = [0..b-1] \\ us ++ [b..] \\ vs
				where (us,vs) = partition (<b) xs 

head (xs++ys) = if null xs then head ys else head xs
-}

minfree xs = minfrom 0 xs

minfrom a (n,xs)	| n == 0 		     = a
					| m == b - a 		 = minfrom b (n-m,vs)
					| otherwise          = minfrom a (m,us)
				  	  where      (us,vs) = partition (<b) xs
					    	     b       = a + 1 + (n / 2)
							     m	     = length us
