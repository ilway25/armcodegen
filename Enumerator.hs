module Enumerator where

import Control.Applicative ((<$>))
import Data.List (inits, sortBy)
import Control.Monad (guard)

import Data.Ord (comparing)

import Data

--------------------------------------------------------------------------------
--- Enumerate and filter out possible representations for prime
--------------------------------------------------------------------------------

enumLimbs :: Prime -> [Limbs]
enumLimbs (Prime m _) =
	map radix [(m + 31) `div` 32 .. 100] -- limit size
  where
	base  n = map (ceiling.((fromIntegral m :: Double) / fromIntegral n *)) [0..]
	radix n = take n $ zipWith (-) (tail $ base n) (base n)

checkConstraint :: Prime -> Limbs -> Bool
checkConstraint (Prime _ k) (a:b:rest) =
	(2 :: Integer) ^ (a + b + c) > (fromIntegral k :: Integer) * 2 ^ (65 :: Integer) -- a stricter bound
  where
  	c = last rest
checkConstraint _ _ = error "Undefined constrant"

genLimbs :: Prime -> [Limbs]
genLimbs p = filter (checkConstraint p) $ enumLimbs p

getLimbs :: Prime -> [Limbs]
getLimbs p@(Prime m _) = do
	l <- genLimbs p
	guard $ all (<= precalculationOverflowBound p) l
	let h = genFormula p l
	let bound = minimum $ map sumOverflowBound h
	guard $ all (<= bound) l
	guard $ sum l == m
	return l

precalculationOverflowBound :: Prime -> Int  -- we assume only *k
precalculationOverflowBound (Prime _ k) = floor $ logBase 2 ((2::Double)**32 / fromIntegral k)

sumOverflowBound :: [Term] -> Int
sumOverflowBound h = floor $ logBase 2 (sqrt ((2::Double)**64 / fromIntegral sumOfCoef))
  where
  	sumOfCoef = foldr helper 0 h
  	helper (Term n _ _) acc = n + acc

genFormula :: Prime -> Limbs -> [[Term]]
genFormula (Prime m k) limbs = mulreduce f g
  where
	nLimbs = length limbs
	radix  = map sum (inits $ cycle limbs)

	mulElements :: Element -> Element -> Term
	mulElements x y = Term (2 ^ n) x y
	  where
	    base = radix !! index x + radix !! index y
	    n = base - last (takeWhile (<= base) radix)

	wrapAround :: Term -> Term
	wrapAround (Term _ x y) = Term n' x y
	  where
	    base = radix !! index x + radix !! index y - m
	    n = base - last (takeWhile (<= base) radix)
	    n' = k * 2 ^ n

	add :: [[Term]] -> [[Term]] -> [[Term]]
	add (x:xs) (y:ys) = (x ++ y) : add xs ys
	add xs@(_:_) _    = xs
	add _ ys@(_:_)    = ys
	add _ _           = []

	multiply :: [Element] -> [Element] -> [[Term]]
	multiply (x:xs) ys'@(y:ys) =
	    [mulElements x y] : add (multiply [x] ys) (multiply xs ys')
	multiply _ _ = []

	reduce :: [[Term]] -> [[Term]]
	reduce xs = add p $ map wrapAround <$> q
	  where
	    (p, q) = splitAt nLimbs xs

	mulreduce :: [Element] -> [Element] -> [[Term]]
	mulreduce x y = reduce $ multiply x y

	f, g :: [Element]
	f = map (Element "f") [0..nLimbs-1]
	g = map (Element "g") [0..nLimbs-1]

-- I only want the first representation!!
getFirst :: Prime -> (Limbs, Formula)
getFirst p = (limbs, genFormula p limbs)
  where
    limbs = head (getLimbs p)

--------------------------------------------------------------------------------
--- Enumerate evaluation order, 3 to 4 limbs as a group
--------------------------------------------------------------------------------

groupByN :: Int -> [a] -> [[a]]
groupByN _ [] = []
groupByN n xs = ys : groupByN n rest
  where (ys, rest) = splitAt n xs

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

evalOrders :: Int -> [[[Int]]]
evalOrders nlimbs = map postProcess [ groupByN 3 [0..nlimbs-1]
                                   , groupByN 4 [0..nlimbs-1]
                                   , step 3
                                   , step 4]
  where
    step  n = evens n ++ odds n
    evens n = groupByN n $ takeWhile (<nlimbs) [0,2..]
    odds  n = groupByN n $ takeWhile (<nlimbs) [1,3..]

    postProcess :: [[a]] -> [[a]]
    postProcess = groupSmallParts . sortBy (comparing length)

    --
    groupSmallParts :: [[a]] -> [[a]]
    groupSmallParts xs@(a:b:rest)
        | length a + length b <= 4 = groupSmallParts $ (a ++ b) : rest
        | otherwise                      = xs
    groupSmallParts xs = xs

mapToTerm :: Formula -> [[[Int]]] -> [[[[Term]]]]
mapToTerm formula = map (map (map (formula!!)))