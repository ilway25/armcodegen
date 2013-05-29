{-# LANGUAGE TemplateHaskell #-}
module LinearScan
    ( LSState (..)
    , initLSState
    , linearScan
    , transform
    , transform2
    ) where

import qualified Data.Map as M
import qualified Data.Bimap as BM

import Control.Applicative ((<$>))
import Control.Monad (forM, forM_, when, unless, foldM)
import Data.List (sortBy, delete, maximumBy, insertBy, delete)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

import qualified Control.Monad.State as S

--import Control.Monad.Trans.Class


import SSA

import Insn

-- MonadState lenses
import Data.Label (mkLabels)
import Data.Label.PureM


lift :: Monad m => t -> m ()
lift _ = return () -- a hack

data LSState = LSState
    { _unhandled :: [(String, UsePoints)]
    , _active :: [(String, UsePoints)]
    , _regMap :: BM.Bimap String Int
    , _posMap :: M.Map Int Int
    , _stores :: [(String, PC)]
    , _loads  :: [(String, PC)]
    , _pcRegMap :: M.Map PC (M.Map String Int)
    }
$(mkLabels [''LSState])

initLSState :: M.Map String UsePoints -> LSState
initLSState uses = LSState intervals [] BM.empty M.empty [] [] M.empty
  where
    intervals  = sortBy (comparing startPoint) $ M.toList uses
    startPoint = head . snd

linearScan :: S.StateT LSState IO ()
linearScan = do
    done <- null <$> gets unhandled
    unless done $ do
        cur:rest <- gets unhandled
        unhandled =: rest

        expireOldIntervals cur

        a <- gets active
        lift $ putStrLn $ "Allocating interval " ++ show cur
        lift $ putStrLn "Active:"
        forM_ a $ \(var, uses) -> do
            Just reg <- BM.lookup var <$> gets regMap
            lift $ putStrLn ('\t' : var ++ "\tr" ++ show reg ++ "\t" ++ show uses)

        tryAllocateFreeReg cur

        lift $ putStrLn ""

        linearScan

  where
    expireOldIntervals cur = do
        let pos = startPoint cur
        act <- gets active
        forM_ act $ \it ->
            when (endPoint it < pos) $ do
                active =. delete it
                lift $ putStrLn $ "Expiring " ++ show it

    tryAllocateFreeReg cur@(var, uses) = do
        resetPosMap
        act <- gets active
        forM_ act $ \(v, _) -> do
            pos <- BM.lookup v <$> gets regMap
            case pos of
                Nothing -> do lift $ print "Impossible A."
                              error "A."
                Just n  -> posMap =. M.insert n 0

        (reg, freePos) <- maximumBy (comparing snd) <$> M.toList <$> gets posMap

        if freePos == 0
            then do
                lift $ putStrLn $ "Allocation failed for " ++ var
                allocateBlcokedReg cur
            else do
                lift $ putStrLn $ "Allocate r" ++ show reg ++ " to " ++ var
                regMap =. BM.insert var reg
                active =. (cur:)

                rm <- BM.toMap <$> gets regMap
                pcRegMap =. M.insert (head uses) rm

    allocateBlcokedReg cur@(var, uses) = do
        resetPosMap
        act <- gets active
        forM_ act $ \(v, u) -> do
            pos <- BM.lookup v <$> gets regMap
            case pos of
                Nothing -> do lift $ print "Impossible B."
                              error "B."
                Just n  -> let actUses = filter (>= head uses) u
                           in unless (null actUses) $
                                posMap =. M.insert n (head actUses)

        (reg, _) <- maximumBy (comparing snd) <$> M.toList <$> gets posMap

        Just varToSplit <- BM.lookupR reg <$> gets regMap
        Just varUses <- lookup varToSplit <$> gets active
        let (uses1, uses2) = span (<startPoint cur) varUses
        active    =. delete (varToSplit, varUses)
        active    =. ((varToSplit, uses1):)
        unhandled =. insertBy (comparing snd) (varToSplit, uses2)

        stores    =. ((varToSplit, last uses1):)
        loads     =. ((varToSplit, head uses2):)

        lift $ putStrLn $ "Allocate r" ++ show reg ++ " to " ++ var ++ ", splitting " ++ varToSplit ++ " before " ++ show (startPoint cur)
        regMap =. BM.insert var reg
        active =. (cur:)

        return ()  -- hack for linter

        rm <- BM.toMap <$> gets regMap
        pcRegMap =. M.insert (head uses) rm

    resetPosMap =
        posMap =: M.fromList [(n, maxBound :: Int) | n <- [0..13]]

    startPoint  = head . snd
    endPoint    = last . snd

data TransState = TransState
                { _prMap :: [(PC, M.Map String Int)]
                } deriving (Show)
$(mkLabels [''TransState])

transform :: [(PC, Insn)] -> M.Map PC (M.Map String Int) -> [IR HWVal]
transform insns prm = let initState = TransState (M.toList prm)
                      in S.evalState f initState
  where
    f = forM insns $ \(pc, insn) -> do
            _:rest <- gets prMap
            when (not (null rest) && fst (head rest) == pc) $ prMap =. tail
            (_,m):_ <- gets prMap
            return $ fmap (mapper m) insn

    mapper m (Var v) = Reg $ fromMaybe (-10000) $ M.lookup v m
    mapper _ (IntValue n) = HWInt n
    mapper _ _ = undefined

-- restore elimination
transform2 :: [(PC, Insn)] -> [(PC, Insn)]
transform2 insns = reverse $ S.evalState f M.empty
  where
    f = foldM g [] insns
    g acc i@(_, insn) =
        case insn of
            Mov (Var s) _ _         -> S.modify (M.insert s False) >> return (i:acc)
            BinOp _ (Var s) _ _ _   -> S.modify (M.insert s False) >> return (i:acc)
            MulOp _ (Var s) (Var t) _ _   -> S.modify (M.insert t False . M.insert s False) >> return (i:acc)

            Load (Location s) (Var t) -> do
                when (s == t) $ S.modify (M.insert s True)
                return (i:acc)

            Store (Location s) (Var t) ->
                if s == t
                    then do
                        result <- M.lookup s <$> S.get
                        case result of
                            Just True -> return acc
                            _ -> return $ i : acc
                    else return (i:acc)


            _ -> return (i:acc)

