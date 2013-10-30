\begin{code}
module BinaryRandomAccessList (BinaryList) where

import Prelude hiding (head, tail, lookup)
import RandomAccessList

data Tree a  = Leaf a | Node Int (Tree a) (Tree a)
data Digit a = Zero | One (Tree a)
newtype BinaryList a = BL [Digit a]

size (Leaf x) = 1
size (Node w t1 t2) = w

link t1 t2 = Node (size t1 + size t2) t1 t2

consTree t  [] = [One t]
consTree t  (Zero : ts) = One t : ts
consTree t1 (One t2 : ts) = Zero : consTree (link t1 t2) ts

unconsTree [] = error "empty list"
unconsTree [One t] = (t, [])
unconsTree (One t:ts) = (t, Zero : ts)
unconsTree (Zero:ts) = (t1, One t2 : ts')
  where (Node _ t1 t2, ts') = unconsTree ts

instance RandomAccessList BinaryList where
  empty = BL []
  isEmpty (BL ts) = null ts

  cons x (BL ts) = BL (consTree (Leaf x) ts)
  head (BL ts) = let (Leaf x, _) = unconsTree ts in x
  tail (BL ts) = let (_, ts') = unconsTree ts in BL ts'

  lookup i (BL ts) = look i ts
    where
      look i [] = error "bad subscript"
      look i (Zero : ts) = look i ts
      look i (One t : ts) =
        if i < size t then lookTree i t
        else look (i - size t) ts

      lookTree 0 (Leaf x) = x
      lookTree i (Leaf x) = error "bad subscript"
      lookTree i (Node w t1 t2) =
        if i < w `div` 2 then lookTree i t1
        else lookTree (i - w `div` 2) t2

  update i y (BL ts) = BL (upd i ts)
    where
      upd i [] = error "bad subscript"
      upd i (Zero : ts) = Zero : upd i ts
      upd i (One t : ts) =
        if i < size t then One (updTree i t) : ts
        else One t : upd (i - size t) ts

      updTree 0 (Leaf x) = Leaf y
      updTree i (Leaf x) = error "bad subscript"
      updTree i (Node w t1 t2) =
        if i < w `div` 2 then Node w (updTree i t1) t2
        else Node w t1 (updTree (i - w `div` 2) t2)
\end{code}
