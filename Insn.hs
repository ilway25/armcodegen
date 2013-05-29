module Insn
    ( Insn
    , IR (..)
    , BinOp (..)
    , MulOp (..)
    , Val (..)
    , HWVal (..)
    , Barrel (..)
    , Mem (..)
    , Used (..)
    , Defd (..)
    , forUsed_
    , forDefd_
    , mapDefd
    , mapUsed
    ) where

import Data.Word (Word32)
import Numeric (showHex)

import Data.Foldable
import Data.Monoid (mempty, (<>))

type Insn = IR Val

-- Note: Would be better to have "Barrel a" (typeclass Barrel).
data IR a = Mov   a a Barrel
          | BinOp BinOp a a a Barrel
          | MulOp MulOp a a a a
          | Store (Mem a) a
          | Load  (Mem a) a

data BinOp  = Add | Adds | Adc | Sub | Subs | Sbc | And | Or
data MulOp  = Mul | Mla

data Val    = Var String | IntValue Word32 | VReg Int
data Barrel = None | SHL Int | SHR Int | ASR Int

data HWVal  = Reg Int | HWInt Word32

data Mem a  = Location String | Ref a Int

--------------------------------------------------------------------------------
--- Fold/map functions for instructions
--------------------------------------------------------------------------------

instance Functor IR where
    fmap f (Mov s a bs)        = Mov   (f s) (f a) bs
    fmap f (BinOp op s a b bs) = BinOp op (f s) (f a) (f b) bs
    fmap f (MulOp Mul s t a b) = MulOp Mul (f s) (f t) (f a) (f b)
    fmap f (MulOp Mla s t a b) = MulOp Mla (f s) (f t) (f a) (f b)
    fmap f (Store m v)         = Store (fmap f m) (f v)
    fmap f (Load  m v)         = Load  (fmap f m) (f v)

newtype Used a = Used (IR a)
newtype Defd a = Defd (IR a)

instance Foldable Used where
    foldMap f (Used x) = h x
      where
        h (Mov _ a _)         = f a
        h (BinOp _ _ a b _)   = f a <> f b
        h (MulOp Mul _ _ a b) = f a <> f b -- !!?
        h (MulOp Mla s t a b) = f s <> f t <> f a <> f b
        h (Store (Ref v _) s) = f v <> f s
        h (Store _ s)         = f s
        h (Load (Ref v _) _)  = f v
        h (Load _ _)          = mempty

instance Foldable Defd where
    foldMap f (Defd x) = h x
      where
        h (Mov s _ _)         = f s
        h (BinOp _ s _ _ _)   = f s
        h (MulOp Mul s t _ _) = f s <> f t
        h (MulOp Mla _ _ _ _) = mempty
        h (Store _ _)         = mempty
        h (Load _ s)          = f s

forUsed_ :: Monad m => IR Val -> (String -> m ()) -> m ()
forUsed_ insns f = forM_ (Used insns) g
  where
    g (Var v) = f v
    g _       = return ()

forDefd_ :: Monad m => IR Val -> (String -> m ()) -> m ()
forDefd_ insns f = forM_ (Defd insns) g
  where
    g (Var v) = f v
    g _       = return ()

mapVar :: (String -> String) -> Val -> Val
mapVar f (Var v) = Var $ f v
mapVar _ x       = x

instance Functor Mem where
    fmap f (Ref v n)    = Ref (f v) n
    fmap _ (Location s) = Location s

mapDefd :: (String -> String) -> Insn -> Insn
mapDefd f = h (mapVar f)
  where
    h g (Mov s a bs)        = Mov   (g s) a bs
    h g (BinOp op s a b bs) = BinOp op (g s) a b bs
    h g (MulOp Mul s t a b) = MulOp Mul (g s) (g t) a b
    h _ (MulOp Mla s t a b) = MulOp Mla s t a b
    h g (Store m v)         = Store (fmap g m) v
    h g (Load  m v)         = Load  (fmap g m) (g v)

mapUsed :: (String -> String) -> Insn -> Insn
mapUsed f = h (mapVar f)
  where
        h g (Mov s a bs)        = Mov   s (g a) bs
        h g (BinOp op s a b bs) = BinOp op s (g a) (g b) bs
        h g (MulOp Mul s t a b) = MulOp Mul s t (g a) (g b)
        h g (MulOp Mla s t a b) = MulOp Mla (g s) (g t) (g a) (g b)
        h g (Store m v)         = Store (fmap g m) (g v)
        h g (Load  m v)         = Load  (fmap g m) v

--------------------------------------------------------------------------------
--- Prettyprinting
--------------------------------------------------------------------------------

instance Show a => Show (IR a) where
    show (Mov   s a bs) = show s ++ " = " ++ showBarrel a bs
    show (BinOp op s a b bs)
        = show s ++ " = " ++ show a ++ " " ++ show op ++ " " ++
          showBarrel b bs ++ showOpExtra op

    show (MulOp op s t a b)
        = show s ++ " " ++ show t ++ showOp op ++ show a ++ " * " ++ show b
      where
        showOp Mul = " = "
        showOp Mla = " += "


    show (Store mem v) = show mem ++ " = " ++ show v
    show (Load mem v)  = show v ++ " = " ++ show mem

instance Show Val where
    show (Var s) = s
    show (IntValue n) = "0x" ++ showHex (fromIntegral n :: Word32) ""
    show (VReg n) = "_r" ++ show n


instance Show HWVal where
    show (Reg n) = 'r' : show (if n == 13 then 14 else n)
    show (HWInt n) = "0x" ++ showHex (fromIntegral n :: Word32) ""

instance Show a => Show (Mem a) where
  show (Location s)       = "[" ++ s ++ "]" -- Refer to a stack slot
  show (Ref v n) = show v ++ "[" ++ show n ++ "]"

instance Show BinOp where
    show Add  = "+"
    show Adds = "+"
    show Adc  = "+"
    show Sub  = "-"
    show Subs = "-"
    show Sbc  = "-"
    show And  = "&"
    show Or   = "|"

showOpExtra :: BinOp -> String
showOpExtra Adds = " (set carry)"
showOpExtra Subs = " (set carry)"
showOpExtra Adc  = " + carry"
showOpExtra Sbc  = " - carry"
showOpExtra _    = ""

showBarrel :: Show a => a -> Barrel -> String
showBarrel b None    = show b
showBarrel b (SHL n) = "(" ++ show b ++ " << " ++ show n ++ ")"
showBarrel b (SHR n) = "(" ++ show b ++ " unsigned>> " ++ show n ++ ")"
showBarrel b (ASR n) = "(" ++ show b ++ " signed>> " ++ show n ++ ")"
