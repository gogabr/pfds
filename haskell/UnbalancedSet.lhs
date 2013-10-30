\begin{code}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module UnbalancedSet (UnbalancedSet) where

import Set

data UnbalancedSet a = E | T (UnbalancedSet a) a (UnbalancedSet a)

instance Ord a => Set UnbalancedSet a where
  empty = E

  member x E = False
  member x (T a y b) = if x < y then member x a
                       else if x > y then member x b
                            else True

  insert x E = T E x E
  insert x s@(T a y b) = if x < y then T (insert x a) y b
                         else if x > y then T a y (insert x b)
                              else s
\end{code}
