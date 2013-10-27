\begin{code}
module CatenableDeque (CatenableDeque(..)) where

import Prelude hiding (head, tail, last, init, (++))
import Deque

class Deque d => CatenableDeque d where
  (++) :: d a -> d a -> da
\end{code}
