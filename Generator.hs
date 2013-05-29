module Generator (Node(..), run, termToNodeTuple) where

import Data

import Data.List
import Data.Ord
import Data.Maybe
import Control.Monad.State

import qualified MultiMap as MM
import qualified Data.Set as Set

type NodeMap = MM.MultiMap Node Node
type FGMaps = (NodeMap, NodeMap)

-- Node is f1, g4, 2f4 or 19g5
data Node = Node Int Element deriving (Eq, Ord)

instance Show Node where
    show (Node 1 e) = show e
    show (Node n e) = '_' : show n ++ show e

termToNodeTuple :: Term -> (Node, Node)
termToNodeTuple (Term k e1 e2) = (Node n e1, Node (k `div` n) e2)
  where
    n = if even k then 2 else 1 -- 38 to 2 * 19

helper1, helper2 :: NodeMap -> [Term] -> NodeMap
helper1 = foldr (uncurry MM.insert . termToNodeTuple)
helper2 = foldr (uncurry (flip MM.insert) . termToNodeTuple)

fgMap, gfMap :: [[Term]] -> NodeMap
fgMap = foldl helper1 MM.empty
gfMap = foldl helper2 MM.empty

mostDominantNode :: NodeMap -> (Node, Set.Set Node)
mostDominantNode = minimumBy (comparing (Set.size . snd)) . MM.toList

maybeHead :: [a] -> Maybe a
maybeHead (x:_) = Just x
maybeHead _     = Nothing

helper :: Set.Set Node -> NodeMap -> Maybe Node
helper gset gfmap = maybeHead $ Set.foldr (\x acc -> Set.toList (MM.lookup x gfmap) ++ acc) [] gset

nextF :: Set.Set Node -> State FGMaps (Node, Set.Set Node)
nextF gs = do
    (fgmap, gfmap) <- get
    let f = fromMaybe (fst $ mostDominantNode fgmap) (helper gs gfmap)
    let gset = MM.lookup f fgmap
    let fgmap' = MM.deleteKey f fgmap
    let gfmap' = Set.foldr (`MM.deleteElem` f) gfmap gset
    put (fgmap', gfmap')
    return (f,gset)

run :: Formula -> [[Term]] -> State FGMaps [(String, Node, Node)]
run hOut h' = do
    put (fgMap h', gfMap h')
    (_, gfmap) <- get
    let gset = MM.keys gfmap
    looper gset []
  where
    looper gset result = do
        (fgmap, _) <- get
        if MM.null fgmap
            then return result
            else do
                ans@(_,gset') <- nextF gset
                looper gset' (builder hOut ans result)

nodesToLimb :: Node -> Node -> [[Term]] -> String
nodesToLimb (Node n1 e1) (Node n2 e2) tss = 'h' :
    show (fromMaybe (-1) $ termToLimb (Term (n1 * n2) e1 e2) tss)

termToLimb :: Term -> [[Term]] -> Maybe Int
termToLimb t = findIndex (not . null . elemIndices t)

builder :: Formula -> (Node, Set.Set Node) -> [(String, Node, Node)] -> [(String, Node, Node)]
builder hOut (f, gset) xs = xs ++ Set.fold (\g acc -> (nodesToLimb f g hOut, f, g):acc) [] gset