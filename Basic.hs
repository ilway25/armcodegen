module Basic (hOut, Term(..), Element(..)) where

import Data.List
import Control.Applicative

data Element = Element { symbol :: String, index :: Int } deriving (Eq, Ord)
data Term    = Term Int Element Element deriving (Eq, Ord)

data Prime = Prime Int Int

instance Show Element where
    show (Element x n) = x ++ show n

instance Show Term where
    show (Term n e1 e2) = show n ++ " " ++ show e1 ++ show e2

instance Show Prime where
    show (Prime m k) = "2^" ++ show m ++ " - " ++ show k

-- # Parameters

--p25519 = Prime 521 1
--limbs = take 17 $ repeat 31

--p25519 = Prime 127 1
--limbs = [26,25,26,25,25]
--limbs = [26,26,26,26,26]

p25519 :: Prime
p25519 = Prime 255 19
limbs :: [Int]
limbs = [26,25,26,25,26,25,26,25,26,25]
--limbs = take 9 $ cycle [29,28,28]
--limbs = [27,25,26,25,26,25,26,25,25,25]
--limbs = [24,23,23,23,23,24,23,23,23,23,23]
--limbs = take 10 $ cycle [27,24]
nLimbs :: Int
nLimbs     = length limbs
radix :: [Int]
radix = map sum (inits $ cycle limbs)


--printParameters = do
--    putStrLn $ "Prime: " ++ show p25519
--    putStrLn $ "Size:  " ++ show nLimbs ++ ", " ++ show (sum limbs)
--    putStrLn $ "Limbs: " ++ show limbs
--    putStrLn $ "Radix: " ++ show (take (2 * nLimbs) radix)
--    print checkRadix

-- * We want a "good" limb representation (deifintion of "good"??)
-- A Wrong check => if n = 10, then in limbs[], for any k <= 5, sum of first(?) k subsequent elements
--                 must >= any k subsequent elements that come after at any position
--                 (it's better to minimize their difference)
-- For example,  [26,25,26,25,26,25,26,25,26,25]
--                ^^^^^^^^          ^^^^^^^^
--              sum of FIRST 3   >=  sum of any 3 subsequent
--checkRadix = all (\(a,b) -> radix !! a + radix !! b >= radix !! (a+b)) [(a,b) | a <- [0..nLimbs-1], b <- [0..nLimbs-1]]

-- # Radix Operations
mulElements :: Element -> Element -> Term
mulElements x y = Term (2 ^ n) x y
  where
    base = radix !! index x + radix !! index y
    n = base - last (takeWhile (<= base) radix)
-- a good limb representation is one where
--   n == base - radix !! (index x + index y)

wrapAround :: Term -> Term
wrapAround (Term _ x y) = Term n' x y
  where
    base = radix !! index x + radix !! index y - m -- a bad limb representation would have base < 0
    n = base - last (takeWhile (<= base) radix)
    n' = k * 2 ^ n
    Prime m k = p25519

-- # Polynomial Multiplication
add :: [[Term]] -> [[Term]] -> [[Term]] -- can be more general [[a]]
add (x:xs) (y:ys) = (x ++ y) : add xs ys
add xs@(_:_) _    = xs
add _ ys@(_:_)    = ys
add _ _           = []

multiply :: [Element] -> [Element] -> [[Term]]
multiply (x:xs) ys'@(y:ys) =
    [mulElements x y] : add (multiply [x] ys) (multiply xs ys')
multiply _ _ = []

-- This assumes a good limb representation
reduce :: [[Term]] -> [[Term]]
reduce xs = add p $ map wrapAround <$> q
  where
    (p, q) = splitAt nLimbs xs

mulreduce :: [Element] -> [Element] -> [[Term]]
mulreduce x y = reduce $ multiply x y

-- # Testing
f, g :: [Element]
f = map (Element "f") [0..nLimbs-1]
g = map (Element "g") [0..nLimbs-1]

hOut :: [[Term]]
hOut = mulreduce f g

--main :: IO()
--main = do
--    printParameters
--    putStrLn "\n===== Input ====="
--    putStrLn $ "f = " ++ show f
--    putStrLn $ "g = " ++ show g
--    let h = multiply f g
--    putStrLn "\n===== Multiplication ====="
--    mapM_ print h
--    putStrLn "\n===== Reduction ====="
--    let h' = reduce h
--    mapM_ print h'
--    putStrLn "\n===== LaTeX Output ====="
--    putStrLn $ printLatex h'

-- # Latex Pretty Printer
--printLatex :: [[Term]] -> String
--printLatex xs = "\\begin{alignat}{" ++ show (n + 1) ++ "}\n" ++ intercalate "\\\\\n" (zipWith printLatex' xs [0..]) ++ "\n\\end{alignat}"

--printLatex' :: [Term] -> Int -> String
--printLatex' xs n = "h_{" ++ show n ++ "} &= & " ++ intercalate "+& & " (map helper xs)
--  where
--    helper' (Element x n) = x ++ "_{" ++ show n ++ "}"
--    helper (Term n e1 e2)
--        | n == 1    = helper' e1 ++ helper' e2
--        | otherwise = show n ++ " " ++ helper' e1 ++ helper' e2