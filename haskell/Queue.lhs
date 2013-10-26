\begin{code}
module Queue (Queue(..)) where

import Prelude hiding (head, tail)

class Queue q where
  empty   :: q a
  isEmpty :: q a -> Bool

  snoc    :: q a -> a -> q a
  head    :: q a -> a
  tail    :: q a -> q a
\end{code}
