\begin{code}
module LazyPairingHeap (PairingHeap) where

import Heap

data PairingHeap a = E | T a (PairingHeap a) (PairingHeap a)

link (T x E m) a = T x a m
link (T x b m) a = T x E (merge (merge a b) m)

instance Heap PairingHeap where
  empty = E
  isEmpty E = True
  isEmpty _ = False

  insert x a = merge (T x E E) a
  merge a E = a
  merge E b = b
  merge a@(T x _ _) b@(T y _ _) = if x <= y
                                  then link a b
                                  else link b a

  findMin E = error "empty heap"
  findMin (T x a m) = x

  deleteMin E = error "empty heap"
  deleteMin (T x a m) = merge a m
\end{code}
