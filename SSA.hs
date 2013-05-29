{-# LANGUAGE TemplateHaskell #-}
module SSA
    ( initSSAState
    , convertToSSA
    , _uses
    , _body --
    , PC
    , UsePoints
    ) where

-- MLA is not in SSA form. (hi lo += s * t)
-- We maybe use "Pseudo-SSA" form, in which each live interval contains
-- only one range, e.g. has no holes, because in straight-line code, there
-- is no phi functions.

import qualified Data.Map as M
import Data.List (nub)

import Insn

import Control.Monad.State (State)
import Control.Monad (when, liftM, forM_)
import Control.Applicative ((<$>))

import Data.Maybe (fromMaybe, isNothing)

-- MonadState lenses
import Data.Label (mkLabels)
import Data.Label.PureM

type PC = Int

type UsePoints = [PC]
data SSAState  = SSAState { _uses  :: M.Map String UsePoints
                          , _count :: M.Map String Int
                          , _body  :: [Insn]  -- Transformed body
                          } deriving (Show)
$(mkLabels [''SSAState])

initSSAState :: SSAState
initSSAState = SSAState M.empty M.empty []

convertToSSA :: [Insn] -> State SSAState ()
convertToSSA insns = do
    forM_ (zip [1 :: PC ..] insns) $ \(pc, insn) -> do
        forUsed_ insn $ \v -> do
            c <- M.lookup v <$> gets count
            when (isNothing c) $ count =. M.insert v 0
            uses =. M.insertWith (++) (append v c) [pc]

        c1 <- gets count
        let ir1 = mapUsed (\v -> append v (M.lookup v c1)) insn

        forDefd_ insn $ \v -> do
            c <- liftM (+1) <$> M.lookup v <$> gets count
            uses =. M.insertWith (++) (append v c) [pc]
            count =. M.insert v (fromMaybe 0 c)

        c2 <- gets count
        let ir2 = mapDefd (\v -> append v (M.lookup v c2)) ir1

        body =. (ir2:)

    -- reverse the order
    body =. reverse
    uses =. M.map (nub . reverse)  -- Note: nub is O(n^2)

  where
    append :: String -> Maybe Int -> String
    append v (Just 0) = v
    append v (Just n) = v ++ "_" ++ show n
    append v Nothing  = v