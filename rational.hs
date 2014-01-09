-- file: rational.hs
import Prelude hiding (and, or, not, pred, succ)
    

infix 5 %%
n %% d = make_rat n d

-- div :: Integral a => a -> a -> a
-- /   :: Fractional a => a -> a -> a 
make_rat n d 
    | same_sig n d = (an `div` g,ad `div` g)
    | otherwise    = ((-an) `div` g, ad `div` g)
    where an = abs n
          ad = abs d
          same_sig x y = (x > 0 && y > 0) || (x<0 && y<0)
          g = gcd_ an ad

gcd_ :: Integral a => a -> a -> a
gcd_ n d | d == 0 = n
         | n < d  = gcd_ d n
         | otherwise = gcd_ d (n `mod` d)

numer x      = fst x
denom x      = snd x



print_rat (n,d) = show n ++ "/" ++ show d

add_rat (n1,d1) (n2,d2) = make_rat (n1*d2+n2*d1) (d1*d2)
sub_rat (n1,d1) (n2,d2) = make_rat (n1*d2-n2*d1) (d1*d2)
mul_rat (n1,d1) (n2,d2) = make_rat (n1*n2) (d1*d2)
div_rat (n1,d1) (n2,d2) = make_rat (n1*d2) (d1*n2)
equal_rat (n1,d1) (n2,d2) = (n1*d2) == (d1*n2) 


idf = \x    -> x
fstf = \x y -> x
sndf = \x y -> y

comp :: (t1 -> t) -> (t2 -> t1) -> (t2 -> t)
comp = \g -> \f -> \x -> g (f x)

apply :: (t1 -> t) -> t1 -> t
-- apply = \f -> (\x -> f x)
apply = \f -> comp f idf

twice :: (t -> t) -> t-> t
--twice = \f -> (\x -> f (f x))
twice = \f -> comp f f
    
thrice :: (t -> t) -> t -> t
--thrice = \f -> (\x -> f (f (f x)))
thrice = \f -> comp f (comp f f)

-- sa = \x.x x
--sa = \x -> x x

-- id id = (\x -> x ) id = id
-- fst fst = (\x y -> x) fst = \y -> fst
-- snd snd = (\x y -> y) snd = \y -> y = id
-- twice twice = (\f x -> f (f x)) twice = \x -> twice (twice x) = comp twice twice
-- sa sa = (\x -> x x) sa = sa sa 
-- from above, the term corresponds to an infinite loop in lambda caculus. it is denoted by the sysmbol 

-- Y = yy
-- yy = \t -> (\x -> t (x x)) (\x -> t (x x))
-- yy t = (\x -> t (x x)) (\x -> t (x x))
--      = t ((\x -> t (x x)) (\x -> t (x x)))
    --  = t (Y t)
    --  = t (t (Y t)) -- infinite loop
{-
factorial = \n -> if n == 1 then 1
                  else n * (factorial (n-1))
-}
-- Y combinator
factorial = tt factorial -- factorial is just what is Y t
tt = \ f n -> if n == 1 then 1
             else n * (f (n - 1))

-- exerciese 2.5
-- let's say what is rational number
-- it satifies the demands
-- if x == make_rat n d
-- then  numer x       n
--      ---------- = -----
--       denom x       d
make_rat2 n d = \m -> m n d
numer2 x      = x $ \a b -> a
denom2 x      = x $ \a b -> b

-- 参考维基百科
-- 给出Cnum的定义：实现自然数到Cnum的转换

-- {-# LANGUAGE RankNTypes #-}

--type Cnum = forall a . (a -> a) -> (a -> a)
type Cnum a =  (a -> a) -> (a -> a)
church :: Integer -> Cnum Integer
church 0 = \f -> \x -> x
church n = \f -> \x -> f (church (n-1) f x)

unchurch :: Cnum Integer -> Integer
unchurch n = n (\x -> x + 1) 0

zero, one, two, three :: Cnum a
zero = \f -> id -- Cnum a
one = id
two = \f -> f . f
three = \f -> f . f . f
-- exerciese 2.6
-- zero = \f x -> x
-- zero = \f -> \z -> z

-- one = \f z -> f z
-- one = \f -> f
-- two = \f z -> f (f z)
-- three = \f z -> f (f (f z))
succ :: Cnum a -> Cnum a 
succ = \n f x -> f (n f x) 
-- add_one = \n f -> f . n f 
-- add_one = \n f z -> f (n f z) 

--plus = \m n f z -> m add_one n f z
plus :: Cnum a -> Cnum a -> Cnum a
plus = \m n f -> m f . n f 
-- plus = \m n -> m succ n
-- succ = \n -> plus n one
-- mul = \m n f z -> m (n f) z
-- mul = \m n -> m (plus n) zero
mul :: Cnum a -> Cnum a -> Cnum a
mul = \m n -> m . n

-- why this signature is correct ? 
-- Cnum a = (a -> a) -> (a -> a)
pow :: Cnum a -> (Cnum a -> Cnum a) -> Cnum a
pow m n = n m 

-- pred :: Cnum a -> Cnum a
pred = \n f z -> n (\g h -> h (g f)) (\u -> z) id

minus = \ m n -> n pred m

-- Church Booleans
-- The benefit of using type synonyms is that it enables us to work at a higher level of abstraction. Rather than working with "a function that takes two arguments of the same type and returns a result of the same type" we simply work with "the Cbool type" just as though it was a built-in type. That's nice.
type Cbool a = a -> a -> a
true, false :: Cbool a
true  = \ a b -> a
false = \ a b -> b

not2 :: Cbool (Cbool a) -> Cbool a
not2 f = f false true

not1 :: Cbool a -> Cbool a
not1 = flip 

and :: Cbool (Cbool a) -> Cbool a -> Cbool a
and = \f g -> f g false

or :: Cbool (Cbool a) -> Cbool a -> Cbool a
or = \f g -> f true g

-- xor true true = false
-- xor false false = false
-- xor true false = true
-- xor false true = true
xor :: Cbool (Cbool a) -> Cbool a -> Cbool a
xor = \f g -> f (not1 g) g

-- predicates
isZero :: Cnum (Cbool a) -> Cbool a
isZero = \n -> n (\x -> false) true

isEQ = \m n -> isZero $ minus m n 





-- predicate x then-clause else-clause
-- so predicate x return Boolean which is a function
-- and = \p q -> p q p
{-
or  = \p q -> p p q
-- This is only a correct implementation if the evaluation strategy is applicative order.
not1 = \p a b -> p b a
-- This is only a correct implementation if the evaluation strategy is normal order.
not2 = \p -> p (\a b -> b) (\a b -> a)

xor = \p q -> p (not q) q
-}


data Nat = Zero | Succ Nat


