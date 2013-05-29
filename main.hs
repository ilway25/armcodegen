module Main where

import Enumerator (getFirst, evalOrders, mapToTerm)
import Data

import Control.Monad.State
import Generator
import Insn
import qualified Data.Set as S
import qualified MultiMap as MM

import Data.Bits (shift)

---
import LinearScan
import ARM
import SSA
import Data.List (insertBy, find)
import Data.Ord (comparing)

---

toMids :: [(String, Node, Node)] -> State (S.Set String) [Insn]
toMids = foldM go []
  where
    go acc (h, f, g) = do
        exists <- gets (S.member h)
        modify (S.insert h)
        return $ acc ++ if exists
            then [MulOp Mla (Var $ h ++ "1") (Var $ h ++ "0") (Var $ show f) (Var $ show g)]
            else [MulOp Mul (Var $ h ++ "1") (Var $ h ++ "0") (Var $ show f) (Var $ show g)]

termToInsns :: Formula -> [[[Term]]] -> [Insn]
termToInsns formula terms = evalState (toMids ans) S.empty
  where
    ans = concatMap (\x -> evalState (run formula x) (MM.empty, MM.empty)) terms

main :: IO ()
main = do
    let prime = p1
    let (limbs, formula) = getFirst prime
    let orders = mapToTerm formula (evalOrders (length limbs))
    let midPart =  map (termToInsns formula) orders

    let haha = map (generateAll prime limbs formula) midPart
    let first = haha !! 3

    --mapM_ print first

    main2 first

    --putStrLn "Done."

    print $ evalOrders (length limbs)

generateAll :: Prime -> Limbs -> Formula -> [Insn] -> [Insn]
generateAll (Prime _ k) limbs formula mid = entry ++ prec1 ++ mid ++ unsignedCarry ++ exit
  where
    nlimbs = length formula

    allNodes = concatMap (\(a,b) -> [a,b]) $ map termToNodeTuple (concat formula)
    nodesToPrecalc = filter (\(Node n _) -> n /= 1) allNodes

    entry = [Load (Location "f") (Var "f"),
             Load (Location "g") (Var "g")]

    prec1 = foldr goF [] [0..nlimbs-1] ++ foldr goG [] [0..nlimbs-1]
      where
        goF n acc = Load (Ref (Var "f") (n * 4)) (Var $ 'f' : show n) :
                    maybe acc (\node -> preCalc node ++ acc) (isIn (Element "f" n))
        goG n acc = Load (Ref (Var "g") (n * 4)) (Var $ 'g' : show n) :
                    maybe acc (\node -> preCalc node ++ acc) (isIn (Element "g" n))

    isIn e = find (\(Node _ e') -> e' == e) nodesToPrecalc
    preCalc n@(Node 2 e) = [BinOp Add (Var $ show n) (Var $ show e) (Var $ show e) None]
    preCalc n@(Node 5 e) = [BinOp Add (Var $ show n) (Var $ show e) (Var $ show e) (SHL 2)]
    preCalc n@(Node 19 e) = [ BinOp Add (Var $ show n) (Var $ show e) (Var $ show e) (SHL 3)
                            , BinOp Add (Var $ show n) (Var $ show e) (Var $ show n) (SHL 1)]
    preCalc n = error $ "Precalc Constant Error: " ++ show n

    unsignedCarry = foldr carry [] [0..nlimbs-2] ++
                    [Mov (Var "h01") (IntValue 0) None] ++
                    carryLast (nlimbs - 1) ++
                    foldr carry [] [0]
      where
        carry n acc = [ BinOp Adds h10 h10 h00 (SHR limbsize)
                      , BinOp Adc  h11 h11 (IntValue 0) None
                      , BinOp And h00 h00 (IntValue $ shift 1 limbsize - 1) None
                      , BinOp Adds h10 h10 h01 (SHL (32-limbsize))
                      , BinOp Adc  h11 h11 (IntValue 0) None
                      , BinOp Add h11 h11 h01 (SHR limbsize) --------------- ??
                      ] ++ acc
            where
                limbsize = limbs !! n
                h00 = Var $ 'h' : show n ++ "0"
                h01 = Var $ 'h' : show n ++ "1"
                h10 = Var $ 'h' : show (n + 1) ++ "0"
                h11 = Var $ 'h' : show (n + 1) ++ "1"

        carryLast n = Mov (Var "tmp90") h90 (SHR limbsize) :
                      preCalc (Node k h90e) ++
                      [ BinOp Adds h00 h00 (Var $ show (Node k h90e)) None
                      , BinOp Adc h01 h01 (IntValue 0) None
                      , BinOp And h90 h90 (IntValue $ shift 1 limbsize - 1) None
                      ,  Mov (Var "tmp91") h91 (SHL (32-limbsize))] ++
                      preCalc (Node k h91e) ++
                      [ BinOp Adds h00 h00 (Var  $ show (Node k h91e)) None
                      , BinOp Adc  h01 h01 (IntValue 0) None

                      , Mov (Var "tmp91") h91 (SHR limbsize)] ++
                      preCalc (Node k h91e) ++
                      [ BinOp Add h01 h01 (Var  $ show (Node k h91e)) None] --------------- ??]
            where
                limbsize = limbs !! n
                h00 = Var "h00"
                h01 = Var "h01"
                h90 = Var $ 'h' : show n ++ "0"
                h91 = Var $ 'h' : show n ++ "1"
                h90e = Element "tmp" 90
                h91e = Element "tmp" 91

    -- and write back
    exit = Load (Location "h") (Var "h") : concatMap helper [0..nlimbs - 1]
      where helper n = [Store (Ref (Var "h") (n * 4)) (Var h)]
              where h = 'h' : show n ++ "0"

main2 :: [Insn] -> IO ()
main2 insns = do
    let x = execState (convertToSSA insns) initSSAState



    --putStrLn "Original:"
    --mapM_ print $ _body x

    --mapM_ print $ M.toList $ _uses x

    lsState <- execStateT linearScan $ initLSState (_uses x)

    --putStrLn "Stores:" -- stores after
    --mapM_ (putStrLn . ('\t':) . show) $ _stores lsState

    --putStrLn "Loads:" -- reload before
    --mapM_ (putStrLn . ('\t':) . show) $ _loads lsState

    let body   = zip [1 :: PC ..] $ _body x
    let stores = _stores lsState
    let loads  = _loads lsState

    let stores' = let f (v, pc) = (pc, Store (Location v) (Var v))
                  in map f stores

    let loads' = let f (v, pc) = (pc, Load (Location v) (Var v))
                in map f loads

    let result1 = foldr (insertBy (comparing fst)) body loads' -- insert before
    let result2 = transform2 $ foldr (insertBy (\(a,_) (b,_) -> compare (a + 1) b)) result1 stores' -- insert after

    --let ans = map snd result2
    --putStrLn "\n Allocated:"
    --mapM_ print ans
    --let lv = liveness ans

    --mapM_ print $ zip [1..] $ map S.size lv

    --let y = execState (convertToSSA ans) initSSAState
    --lsState2 <- execStateT linearScan $ initLSState (_uses y)

    let finalans = transform result2 (_pcRegMap lsState)

    --mapM_ print finalans

    --putStrLn "\n Transformed:"
    mapM_ putStrLn $ emitARM finalans

    --putStrLn "Done."