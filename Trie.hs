
-- | Character tries. Used for LaTeX input mode

{-# LANGUAGE BangPatterns #-}
module Trie where

--------------------------------------------------------------------------------

import Data.Char

import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)

--------------------------------------------------------------------------------

data Trie a
  = Node !(Maybe a) !(IntMap (Trie a))
  | Leaf !a 
  | Empty

--------------------------------------------------------------------------------

singleton :: String -> a -> Trie a
singleton key value = go key where
  go []     = Leaf value
  go (c:cs) = Node Nothing $ IntMap.singleton (ord c) (go cs)

mbSingleton :: Trie a -> Maybe (String,a)
mbSingleton trie = case toList trie of 
  [x] -> Just x
  _   -> Nothing

--------------------------------------------------------------------------------

toList :: Trie a -> [(String,a)]
toList = go "" where
  go !prefix !trie = case trie of
    Empty   -> []
    Leaf y  -> [(prefix,y)]
    Node (Just y) subtrie -> (prefix,y) : go prefix (Node Nothing subtrie)
    Node Nothing  subtrie -> concat
      [ go (prefix++[chr o]) t | (o,t) <- IntMap.assocs subtrie ]

--------------------------------------------------------------------------------

lookup :: String -> Trie a -> Maybe a
lookup key trie = case lookup' key trie of
  Nothing    -> Nothing
  Just (_,y) -> Just y

lookupCompletion_ :: String -> Trie a -> Maybe a
lookupCompletion_ key trie = fmap snd (lookupCompletion key trie)

lookupCompletion :: String -> Trie a -> Maybe (String,a)
lookupCompletion key trie = case lookupPrefix key trie of
  Nothing    -> Nothing
  Just trie' -> case Trie.lookup "" trie' of 
    Just y     -> Just (key,y)
    _          -> mbSingleton trie'

-- | Only returns if the key is a unique prefix
lookupUnique :: String -> Trie a -> Maybe a
lookupUnique key trie = case lookup' key trie of
  Just (True,y) -> Just y
  _             -> Nothing
 
-- | returns also whether the result is a unique prefix
lookup' :: String -> Trie a -> Maybe (Bool,a)
lookup' = go where

  go key trie = case key of

    (c:cs) -> case trie of
      Node mb subtrie -> case IntMap.lookup (ord c) subtrie of
        Just next -> go cs next
        Nothing   -> Nothing
      _ -> Nothing

    [] -> case trie of
      Node (Just y) _  -> Just (False,y)
      Leaf y           -> Just (True ,y)
      _                -> Nothing

lookupPrefix :: String -> Trie a -> Maybe (Trie a)
lookupPrefix = go where

  go key trie = case key of

    (c:cs) -> case trie of
      Node mb subtrie -> case IntMap.lookup (ord c) subtrie of
        Just next -> go cs next
        Nothing   -> Nothing
      _ -> Nothing

    [] -> Just trie 

--------------------------------------------------------------------------------

fromList :: [(String,a)] -> Trie a 
fromList kvs = foldl (flip insertPair) Empty kvs

insertPair :: (String,a) -> Trie a -> Trie a
insertPair (k,v) = insert k v

insert :: String -> a -> Trie a -> Trie a
insert !str !x !trie = go str trie where

  go key trie = case key of

    (c:cs) -> case trie of
      Leaf y -> Node (Just y) $ IntMap.singleton (ord c) (singleton cs x)
      Empty  -> Node Nothing  $ IntMap.singleton (ord c) (singleton cs x)
      Node mb subtrie -> case IntMap.lookup (ord c) subtrie of
        Just t  -> Node mb $ IntMap.insert (ord c) (go cs t) subtrie
        Nothing -> Node mb $ IntMap.insert (ord c) (singleton cs x) subtrie

    [] -> case trie of
      Node _ subtrie -> Node (Just x) subtrie
      _              -> Leaf x

--------------------------------------------------------------------------------
