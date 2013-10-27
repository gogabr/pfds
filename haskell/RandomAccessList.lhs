\begin{code}
module RandomAccessList (RandomAccessList(..)) where

import Prelude hiding (head, tail, lookup)

class RandomAccessList r where
  empty   :: r a
  isEmpty :: r a -> Bool

  cons    :: a   -> r a -> r a
  head    :: r a -> a
  tail    :: r a -> r a

  lookup  :: Int -> r a -> a
  update  :: Int -> a   -> r a -> r a
\end{code}
