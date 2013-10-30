\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module TrieOfTrees (Tree(..), Trie) where

import Prelude hiding (lookup)
import FiniteMap

data Tree a = E | T a (Tree a) (Tree a)
data Trie mk ks a = Trie (Maybe a) (mk (Trie mk ks (Trie mk ks a)))

instance FiniteMap m k => FiniteMap (Trie (m k)) (Tree k) where
  empty = Trie Nothing empty

  lookup E (Trie v m) = v
  lookup (T k a b) (Trie v m) =
    lookup k m  >>= \m'  ->
    lookup a m' >>= \m'' ->
    lookup b m''

  bind E x (Trie v m) = Trie (Just x) m
  bind (T k a b) x (Trie v m) =
    let tt = case lookup k m of
          Just tt -> tt
          Nothing -> empty
        t = case lookup a tt of
          Just t  -> t
          Nothing -> empty
        t' = bind b x t
        tt' = bind a t' tt
    in Trie v (bind k tt' m)
\end{code}
