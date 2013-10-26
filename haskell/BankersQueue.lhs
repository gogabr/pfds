\begin{code}
module BankersQueue (BankersQueue) where

import Prelude hiding (head, tail)
import Queue

data BankersQueue a = BQ Int [a] Int [a]

check lenf f lenr r =
  if lenr <= lenf
  then BQ lenf f lenr r
  else BQ (lenf + lenr) (f ++ reverse r) 0 []

instance Queue BankersQueue where
  empty = BQ 0 [] 0 []
  isEmpty (BQ lenf f lenr r) = (lenf == 0)

  snoc (BQ lenf f lenr r) x = check lenf f (lenr + 1) (x:r)

  head (BQ lenf []     lenr r) = error "empty queue"
  head (BQ lenf (x:f') lenr r) = x

  tail (BQ lenf []     lenr r) = error "empty queue"
  tail (BQ lenf (x:f') lenr r) = check (lenf - 1) f' lenr r
\end{code}
