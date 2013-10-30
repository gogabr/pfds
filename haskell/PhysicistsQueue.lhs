\begin{code}
module PhysicistsQueue (PhysicistsQueue) where

import Prelude as P
import Queue

data PhysicistsQueue a = PQ [a] Int [a] Int [a]

check w lenf f lenr r =
  if lenr <= lenf
  then checkw w lenf f lenr r
  else checkw f (lenf + lenr) (f ++ reverse r) 0 []

checkw [] lenf f lenr r = PQ f lenf f lenr r
checkw  w lenf f lenr r = PQ w lenf f lenr r

instance Queue PhysicistsQueue where
  empty = PQ [] 0 [] 0 []
  isEmpty (PQ w lenf f lenr r) = (lenf == 0)

  snoc (PQ w lenf f lenr r) x = check w lenf f (lenr+1) (x:r)

  head (PQ []    lenf f lenr r) = error "empty queue"
  head (PQ (x:w) lenf f lenr r) = x

  tail (PQ []    lenf f lenr r) = error "empty queue"
  tail (PQ (x:w) lenf f lenr r) = check w (lenf-1) (P.tail f) lenr r
\end{code}
