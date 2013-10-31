\begin{code}
module CatenableDeque (
  CatenableDeque(..),
  Deque(..)
  ) where

import Prelude hiding (head, tail, last, init, (++))
import Deque

class Deque d => CatenableDeque d where
  (++) :: d a -> d a -> d a
\end{code}
