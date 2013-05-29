module Eval where

import qualified Data.Map as M
import Data.Word (Word32)
import Data.Int  (Int32)
import Data.Bits (shiftL, shiftR)

import Data.Maybe (fromMaybe)

import Insn


type VarEnv = M.Map String Word32
type MemEnv = M.Map String [Word32]

type Env = (VarEnv, MemEnv)

getVal :: Val -> VarEnv -> Word32
getVal (IntValue a) _ = a
getVal (Var a) env = fromMaybe (error $ "panic: not in env: " ++ show a ++ "\n" ++ show env) $ M.lookup a env

getVal' :: Val -> Barrel -> VarEnv -> Word32
getVal' v None env = getVal v env
getVal' v (SHL n) env = getVal v env `shiftL` n
getVal' v (ASR n) env = fromIntegral $ (fromIntegral (getVal v env) :: Int32) `shiftR` n
getVal' v (SHR n) env = getVal v env `shiftR` n

eval :: Insn -> Env -> Env
eval (Mov (Var s) a bs) (env, mem) = (M.insert s (getVal' a bs env) env, mem)
eval (BinOp op (Var s) a b bs) (env, mem) = (M.insert s (evalOp op (getVal a env) (getVal' b bs env)) env, mem)

evalOp :: BinOp -> Word32 -> Word32 -> Word32
evalOp = undefined
    --show Add  = "+"
    --show Adds = "+"
    --show Adc  = "+"
    --show Sub  = "-"
    --show Subs = "-"
    --show Sbc  = "-"
    --show And  = "&"
    --show Or   = "|"

{-
getVal :: Val -> M.Map String Int32 -> Int32
getVal (IntValue a) _ = a
getVal (Var a) env = fromMaybe (error $ "panic: not in env: " ++ show a ++ "\n" ++ show env) $ M.lookup a env

getVal' :: Val -> Barrel -> M.Map String Int32 -> Int32
getVal' v None env = getVal v env
getVal' v (SHL n) env = getVal v env `shiftL` n
getVal' v (ASR n) env = getVal v env `shiftR` n
getVal' v (SHR n) env = fromIntegral $ (fromIntegral (getVal v env) :: Word32) `shiftR` n

evalIR (Mov (Var s) a bs) env = M.insert s (getVal' a bs env) env

evalIR (Add (Var s) a b bs) env = M.insert s (getVal a env + getVal' b bs env) env
evalIR (Sub (Var s) a b bs) env = M.insert s (getVal a env - getVal' b bs env) env

evalIR (And (Var s) a b bs) env = M.insert s (getVal a env .&. getVal' b bs env) env
evalIR (Or (Var s) a b bs) env = M.insert s (getVal a env .|. getVal' b bs env) env
-}