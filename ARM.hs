module ARM where

import Insn

import Data.Word (Word32)
import Numeric (showHex)

import Data.List (elemIndex)

makeStack :: [IR HWVal] -> [String]
makeStack = foldr go []
  where
    go (Store (Location s) _) acc = if s `elem` acc then acc else s : acc
    go (Load (Location s) _) acc = if s `elem` acc then acc else s : acc
    go _ acc = acc

surround :: [String] -> [String] -> [String]
surround stack insns  = [".fpu neon",
                    ".text",
                    ".align 2",
                    ".global _fe_mul_arm11",
                    ".global fe_mul_arm11",
                    "_fe_mul_arm11:",
                    "fe_mul_arm11:",
                    "sub sp,sp,#1000",
                    "str r4,[sp,#696]",
                    "str r5,[sp,#700]",
                    "str r6,[sp,#704]",
                    "str r7,[sp,#708]",
                    "str r8,[sp,#712]",
                    "str r9,[sp,#716]",
                    "str r10,[sp,#720]",
                    "str r11,[sp,#724]",
                    "str r14,[sp,#728]"] ++

                    ["str r0, [sp, #" ++ show (maybe (-99999) (*4) ("h" `elemIndex` stack)) ++ "]",
                    "str r1, [sp, #" ++ show (maybe (-99999) (*4) ("f" `elemIndex` stack)) ++ "]",
                    "str r2, [sp, #" ++ show (maybe (-99999) (*4) ("g" `elemIndex` stack)) ++ "]"]

                    ++ insns ++

                    ["ldr r4,[sp,#696]",
                    "ldr r5,[sp,#700]",
                    "ldr r6,[sp,#704]",
                    "ldr r7,[sp,#708]",
                    "ldr r8,[sp,#712]",
                    "ldr r9,[sp,#716]",
                    "ldr r10,[sp,#720]",
                    "ldr r11,[sp,#724]",
                    "ldr r14,[sp,#728]",
                    "add sp,sp,#1000",
                    "bx lr"]

emitARM :: [IR HWVal] -> [String]
emitARM insns = surround stack $ map emitInsn insns
  where
    stack = makeStack insns

    emitInsn (Mov   s a bs) = "mov " ++ emitVal s ++ ", " ++ emitVal a ++ emitBS bs
    emitInsn (BinOp op s a b bs) = emitOp op ++ " " ++ emitVal s ++ ", " ++ emitVal a ++ ", " ++ emitVal b ++ emitBS bs
    emitInsn (MulOp Mul s t a b) = "umull " ++ emitVal t ++ ", " ++ emitVal s ++ ", " ++ emitVal b ++ ", " ++ emitVal a
    emitInsn (MulOp Mla s t a b) = "umlal " ++ emitVal t ++ ", " ++ emitVal s ++ ", " ++ emitVal b ++ ", " ++ emitVal a
    emitInsn (Store mem v) = "str " ++ emitVal v ++ ", " ++ emitMem mem
    emitInsn (Load mem v) = "ldr " ++ emitVal v ++ ", " ++ emitMem mem

    emitVal (Reg n) = 'r' : show (if n == 13 then 14 else n)
    emitVal (HWInt n) = "#0x" ++ showHex (fromIntegral n :: Word32) ""

    emitBS None    = ""
    emitBS (SHL n) = ", LSL #" ++ show n
    emitBS (SHR n) = ", LSR #" ++ show n
    emitBS (ASR n) = ", ASR #" ++ show n

    emitOp Add  = "add"
    emitOp Adds = "adds"
    emitOp Adc  = "adc"
    emitOp Sub  = "sub"
    emitOp Subs = "subs"
    emitOp Sbc  = "sbc"
    emitOp And  = "and"
    emitOp Or   = "orr"

    emitMem (Location s) = "[sp, #" ++ show (maybe (-99999) (*4) (s `elemIndex` stack)) ++ "]"
    emitMem (Ref v n)    = "[" ++ show v ++ ", #" ++ show n ++ "]"