-- ----------------------------------------------------------------------------
{- |
  Module     : Holumbus.Data.MultiMap
  Copyright  : Copyright (C) 2008 Stefan Schmidt
  License    : MIT

  Maintainer : Stefan Schmidt (stefanschmidt@web.de)
  Stability  : experimental
  Portability: portable
  Version    : 0.1

  This module provides a MultiMap, that means a Map, which can hold
  multiple values for one key, but every distinct value is only stores once.
  So adding the same key-value-pair twice will only create one new entry in
  the map.

  This Map is helpfull to examine how many different key-values-pairs you
  have in your application.

  Most of the functions are borrowed from Data.Map
-}
-- ----------------------------------------------------------------------------

module MultiMap
(
  MultiMap
, empty
, null
, insert
, insertSet
, insertKeys
, lookup
, keys
, elems
, filterElements
, member
, delete
, deleteKey
, deleteElem
, deleteAllElems
, fromList
, fromTupleList
, toList
, toAscList
)
where

import           Prelude hiding (null, lookup)

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Arrow (second)
import Data.Maybe (fromMaybe)

-- | A MultiMap, it can hold more (different!!!) Elements for one key.
data MultiMap k a = MM (Map.Map k (Set.Set a))
  deriving (Eq, Ord)

instance (Show k, Show a) => Show (MultiMap k a) where
  show (MM m) = show $ map (second Set.toList) (Map.toList m)

{-
instance (Show k, Show a) => Show (MultiMap k a) where
  show (MM m) = msShow
    where
    ms = map (\(k,s) -> (k, Set.toList s)) (Map.toList m)
    msShow = concat $ map (\(k,s) -> (show k) ++ "\n" ++ (showL s)) ms
    showL ls = concat $ map (\s -> show s ++ "\n") ls
-}

-- | The empty MultiMap.
empty :: (Ord k, Ord a) => MultiMap k a
empty = MM Map.empty


-- | Test, if the MultiMap is empty.
null :: (Ord k, Ord a) => MultiMap k a -> Bool
null (MM m) = Map.null m


-- | Inserts an element in the MultiMap.
insert :: (Ord k, Ord a) => k -> a -> MultiMap k a -> MultiMap k a
insert k a (MM m) = MM $ Map.alter altering k m
  where
  altering Nothing = Just $ Set.singleton a
  altering (Just s) = Just $ Set.insert a s


-- | Inserts multiple elements in a set to the MultiMap.
insertSet :: (Ord k, Ord a) => k -> Set.Set a -> MultiMap k a -> MultiMap k a
insertSet k newSet mm@(MM m) =
  if Set.null newSet then mm else MM $ Map.alter altering k m
  where
  altering Nothing = Just newSet
  altering (Just s) = Just $ Set.union newSet s


-- | Inserts multiple keys with the same values.
insertKeys :: (Ord k, Ord a) => [k] -> Set.Set a -> MultiMap k a -> MultiMap k a
insertKeys ks a m = foldl (\m' k -> insertSet k a m') m ks


-- | Gets all different elements for one key or an empty set.
lookup :: (Ord k, Ord a) => k -> MultiMap k a -> Set.Set a
lookup k (MM m) = fromMaybe Set.empty (Map.lookup k m)


-- | Get all different elements from a list of keys.
lookupKeys :: (Ord k, Ord a) => [k] -> MultiMap k a -> Set.Set a
lookupKeys ks m = Set.unions $ map (`lookup` m) ks


-- | Get all different keys from the map.
keys :: (Ord k, Ord a) => MultiMap k a -> Set.Set k
keys (MM m) = Set.fromList $ Map.keys m


-- | Get all different values in the map without regarding their keys.
elems :: (Ord k, Ord a) => MultiMap k a -> Set.Set a
elems (MM m) = Set.unions $ Map.elems m


-- | Like lookup keys, but an empty input list will give all elements back,
--   not the empty set.
filterElements :: (Ord k, Ord a) => [k] -> MultiMap k a -> Set.Set a
filterElements [] m = elems m  -- get all
filterElements ks m = lookupKeys ks m


-- | Test, if a key is in the Map.
member :: (Ord k, Ord a) => k -> MultiMap k a -> Bool
member k m = Set.empty /= lookup k m


-- | Deletes an Element from the Map, if the data in Nothing, the whole key is
--   deleted.
delete :: (Ord k, Ord a) => k -> Maybe a -> MultiMap k a -> MultiMap k a
delete k Nothing m = deleteKey k m
delete k (Just a) m = deleteElem k a m


-- | Deletes a whole key from the map.
deleteKey :: (Ord k, Ord a) => k -> MultiMap k a -> MultiMap k a
deleteKey k (MM m) = MM $ Map.delete k m


-- | Deletes a single Element from the map.
deleteElem :: (Ord k, Ord a) => k -> a -> MultiMap k a -> MultiMap k a
deleteElem k a (MM m) = MM $ Map.alter delSet k m
  where
  delSet Nothing = Nothing
  delSet (Just set) = filterEmpty $ Set.delete a set
  filterEmpty set
    | set == Set.empty = Nothing
    | otherwise = Just set


-- | Deletes all Elements (*,a) (slow!!!).
deleteAllElems :: (Ord k, Ord a) => a -> MultiMap k a -> MultiMap k a
deleteAllElems a m = foldl (\m'' k -> deleteElem k a m'') m ks
  where
  ks = Set.toList $ keys m


-- | Creates a MultiMap from a list of pairs (key,set value).
fromList :: (Ord k, Ord a) => [(k,Set.Set a)] -> MultiMap k a
fromList = foldl (\m (k,as) -> insertSet k as m) empty


-- | Creates a MultiMap from a list of tuples.
fromTupleList :: (Ord k, Ord a) => [(k,a)] -> MultiMap k a
fromTupleList = foldl (\m (k,a) -> insert k a m) empty


-- | Transforms a MultiMap to a list of pairs (key,set value).
toList :: (Ord k, Ord a) => MultiMap k a -> [(k,Set.Set a)]
toList (MM m) = Map.toList m


-- | The same as toList, but the keys are in ascending order.
toAscList :: (Ord k, Ord a) => MultiMap k a -> [(k,Set.Set a)]
toAscList (MM m) = Map.toAscList m