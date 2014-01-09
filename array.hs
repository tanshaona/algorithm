import Data.Array

import Data.Ix

mkArray f bnds = array bnds [(i,f i) | i <- range bnds]

fibs n = a where a = array (0,n) ([(0,1),(1,1)] ++
							[(i,a!(i-2)+a!(i-1)) | i <- [2..n]]		
							)

hlist bnds is = accumArray (+) 0 bnds [(i,1) | i <- is, inRange bnds i]

swapRows i i' a = a // ( [( (i,j),a!(i',j) ) | j <- [iLo..jHi]] ++ [ ( (i',j),a!(i,j) ) | j <- [iLo..jHi] ] )
					where ( (iLo,iHo),(iHi,jHi) ) = bounds a

matMult x y = array resultBounds 
					[ ( (i,j), sum [x!(i,k)*y!(k,j) | k <- range (lj,uj)] )
									| i <- range (li,ui),
									  j <- range (lj',uj')	]
			where
 			  ((li,lj),(ui,uj))     = bounds x
			  ((li',lj'),(ui',uj')) = bounds y
			  resultBounds
					| (lj,uj) == (li',ui')   = ((li,lj'),(ui,uj'))
					| otherwise 			 = error "matMult: incompatible bounds"

