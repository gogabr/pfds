\begin{code}
module BootstrapHeap (BootstrapHeap) where

import Heap

data BootstrapHeap h a = E | H a (h (BootstrapHeap h a))

instance Eq a => Eq (BootstrapHeap h a) where
  (H x _) == (H y _) = (x == y)

instance Ord a => Ord (BootstrapHeap h a) where
  (H x _) <= (H y _) = (x <= y)

instance Heap h => Heap (BootstrapHeap h) where
  empty = E
  isEmpty E = True
  isEmpty _ = False

  insert x h = merge (H x empty) h

  merge E h = h
  merge h E = h
  merge h1@(H x p1) h2@(H y p2) =
    if x <= y
    then H x (insert h2 p1)
    else H y (insert h1 p2)


  findMin E = error "empty heap"
  findMin (H x p) = x

  deleteMin E = error "empty heap"
  deleteMin (H x p) =
    if isEmpty p then E
    else let H y p1 = findMin p
             p2 = deleteMin p
         in H y (merge p1 p2)
\end{code}
