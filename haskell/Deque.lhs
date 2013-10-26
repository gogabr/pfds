\begin{code}
module Deque (Deque(..)) where

import Prelude hiding (head, tail, last, init)

class Deque q where
  empty   :: q a
  isEmpty :: q a -> Bool

  cons    :: a -> q a -> q a
  head    :: q a -> a
  tail    :: q a -> q a

  snoc    :: q a -> a -> q a
  last    :: q a -> a
  init    :: q a -> q a
\end{code}
