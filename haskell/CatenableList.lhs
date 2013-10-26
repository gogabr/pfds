\begin{code}
module CatenableList (CatenableList(..)) where

import Prelude hiding (head, tail, (++))

class CatenableList c where
  empty   :: c a
  isEmpty :: c a -> Bool

  cons    :: a   -> c a -> c a
  snoc    :: c a -> a   -> c a
  (++)    :: c a -> c a -> c a

  head    :: c a -> a
  tail    :: c a -> c a
\end{code}
