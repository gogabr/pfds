\begin{code}
module BinomialHeap (BinomialHeap) where

import Heap

data Tree a = Node Int a [Tree a]
newtype BinomialHeap a = BH [Tree a]

rank (Node r x c) = r
root (Node r x c) = x

link t1@(Node r x1 c1) t2@(Node _ x2 c2) =
  if x1 <= x2
  then Node (r+1) x1 (t2 : c1)
  else Node (r+1) x2 (t1 : c2)

insTree t [] = [t]
insTree t ts@(t' : ts') =
  if rank t < rank t' then t:ts else insTree (link t t') ts'

mrg ts1 [] = ts1
mrg [] ts2 = ts2
mrg ts1@(t1:ts'1) ts2@(t2:ts'2)
  | rank t1 < rank t2 = t1 : mrg ts'1 ts2
  | rank t2 < rank t1 = t2 : mrg ts1 ts'2
  | otherwise = insTree (link t1 t2) (mrg ts'1 ts'2)

removeMinTree [] = error "empty heap"
removeMinTree [t] = (t, [])
removeMinTree (t:ts) =
  if root t < root t' then (t, ts) else (t', t:ts')
  where (t', ts') = removeMinTree ts

instance Heap BinomialHeap where
  empty = BH []
  isEmpty (BH ts) = null ts

  insert x (BH ts) = BH (insTree (Node 0 x []) ts)
  merge (BH ts1) (BH ts2) = BH (mrg ts1 ts2)
  findMin (BH ts) = root t
    where (t, _) = removeMinTree ts

  deleteMin (BH ts) = BH (mrg (reverse ts1) ts2)
    where (Node _ x ts1, ts2) = removeMinTree ts
\end{code}
