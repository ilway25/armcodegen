module CodeGen (insns, toMids) where

import Control.Monad.State

import qualified MultiMap as MM
import qualified Data.Set as Set

import Basic
import Generator
import Insn

import Data.Bits (shift, complement)
toMids :: [(String, Node, Node)] -> State (Set.Set String) [Insn]
toMids = foldM go []
  where
    go acc (h, f, g) = do
        exists <- gets (Set.member h)
        modify (Set.insert h)
        return $ acc ++ if exists
            then [MulOp Mla (Var $ h ++ "1") (Var $ h ++ "0") (Var $ show f) (Var $ show g)]
            else [MulOp Mul (Var $ h ++ "1") (Var $ h ++ "0") (Var $ show f) (Var $ show g)]

preCalculation :: [Insn]
preCalculation = cal2f [1 :: Int,3,5,7,9] ++ cal19g [1..9 :: Int]
  where
    cal2f = foldr go1 []
    cal19g = foldr go2 []
    go1 x acc = BinOp Add (Var $ "2f" ++ show x) (Var $ 'f' : show x) (Var $ 'f' : show x) None : acc
    go2 x acc = BinOp Add (Var $ "19g" ++ show x) (Var $ 'g' : show x) (Var $ 'g' : show x) (SHL 3) :
                BinOp Add (Var $ "19g" ++ show x) (Var $ 'g' : show x) (Var $ "19g" ++ show x) (SHL 1) : acc


-- in this prototype we have omitted "carry*19" part!!!!!!!!!!!!
-- finally it "seems" to work.......
carry :: (Int,Int,Int) -> [Insn]
carry (h0, h1, sh) = [BinOp Adds (Var "c0") (Var $ 'h' : show h0 ++ "0") (IntValue $ 1 `shift` (sh - 1)) None,
                      BinOp Adc (Var "c1") (Var $ 'h' : show h0 ++ "1") (IntValue 0) None,
                      BinOp And (Var "c0") (Var "c0") (IntValue (complement $ shift 1 sh - 1)) None,
                      BinOp Subs (Var $ 'h' : show h0 ++ "0") (Var $ 'h' : show h0 ++ "0") (Var "c0") None,
                      BinOp Sbc (Var $ 'h' : show h0 ++ "1") (Var $ 'h' : show h0 ++ "1") (Var "c1") None,
                      Mov (Var "t") (Var "c0") (SHR sh),
                      BinOp Or (Var "t") (Var "t") (Var "c1") (SHL $ 32-sh),
                      BinOp Adds (Var $ 'h' : show h1 ++ "0") (Var $ 'h' : show h1 ++ "0") (Var "t") None,
                      BinOp Adc (Var $ 'h' : show h1 ++ "1") (Var $ 'h' : show h1 ++ "1") (Var "c1") (ASR sh)]

insns :: [Insn]
insns = entry ++ preCalculation ++ muls ++ carries ++ writeBack ++ exit
  where
    h1 = [hOut!!2, hOut!!4, hOut!!6, hOut!!8]
    h2 = [hOut!!3, hOut!!5, hOut!!7, hOut!!9]
    h3 = take 2 hOut -- [hOut!!0, hOut!!1]
    a1 = evalState (run h1) (MM.empty, MM.empty)
    a2 = evalState (run h2) (MM.empty, MM.empty)
    a3 = evalState (run h3) (MM.empty, MM.empty)
    ans = a1 ++ a2 ++ a3
    muls = evalState (toMids ans) Set.empty

    -- better schedule load of f and g (for ex, load on fInsnst use)
    entry =  [Load (Location "input_1") (Var "input_1"),
              Load (Location "input_2") (Var "input_2")]
          ++ concatMap helper [0..9]
      where helper n = [Load (Ref (Var "input_1") (n * 4)) (Var f), Load (Ref (Var "input_2") (n * 4)) (Var g)]
              where f = 'f' : show n
                    g = 'g' : show n

    exit = Load (Location "input_0") (Var "input_0") : concatMap helper [0..9]
      where helper n = [Store (Ref (Var "input_0") (n * 4)) (Var h)]
              where h = 'h' : show n

    carries = concatMap carry [(0,1,26),(4,5,26),(1,2,25),(5,6,25),
                               (2,3,26),(6,7,26),(3,4,25),(7,8,25),
                               (4,5,26),(8,9,26)]

    writeBack = map helper [0..9::Int]
      where
        helper x = Mov (Var $ 'h' : show x) (Var $ 'h' : show x ++ "0") None

