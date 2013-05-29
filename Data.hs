module Data where

import Data.List (intercalate)

data Element = Element { symbol :: String, index :: Int } deriving (Eq, Ord)
data Term    = Term Int Element Element deriving (Eq, Ord)

data Prime = Prime Int Int

instance Show Element where
    show (Element x n) = x ++ show n

instance Show Term where
    show (Term n e1 e2) = show n ++ " " ++ show e1 ++ show e2

instance Show Prime where
    show (Prime m k) = "2^" ++ show m ++ " - " ++ show k

p25519, p1305, p1271, p5211, p25531 :: Prime
p25519 = Prime 255 19
p1305  = Prime 130 5
p1271  = Prime 127 1
p5211  = Prime 521 1  -- limb size may be too large (payoff)
p25531 = Prime 255 31

p1, p2, p3, p4, p5 :: Prime
p1 = p25519
p2 = p1305
p3 = p1271
p4 = p5211
p5 = p25531

type Limbs = [Int]
type Formula = [[Term]]

printLatex :: Limbs -> [[Term]] -> String
printLatex limbs xs = "\\begin{alignat}{" ++ show (length limbs + 1) ++ "}\n" ++ intercalate "\\\\\n" (zipWith printLatex' xs [0..]) ++ "\n\\end{alignat}"
  where
	printLatex' :: [Term] -> Int -> String
	printLatex' xs' nn = "h_{" ++ show nn ++ "} &= & " ++ intercalate "+& & " (map helper xs')
	  where
	    helper' (Element x n) = x ++ "_{" ++ show n ++ "}"
	    helper (Term n e1 e2)
	        | n == 1    = helper' e1 ++ helper' e2
	        | otherwise = show n ++ " " ++ helper' e1 ++ helper' e2