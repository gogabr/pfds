\begin{code}
module BankersDeque (BankersDeque) where

import Prelude hiding (head, tail, last, init)
import Deque

data BankersDeque a = BD Int [a] Int [a]

c = 3

check lenf f lenr r =
  if lenf > c*lenr + 1 then
    let i  = (lenf + lenr) `div` 2
        j  = lenf + lenr - i
        f' = take i f
        r' = r ++ reverse (drop i f)
    in BD i f' j r'
  else if lenr > c*lenf + 1 then
         let j  = (lenf+lenr) `div` 2
             i  = lenf + lenr - i
             r' = take j r
             f' = f ++ reverse (drop j r)
         in BD i f' j r'
       else BD lenf f lenr r

instance Deque BankersDeque where
  empty = BD 0 [] 0 []
  isEmpty (BD lenf f lenr r) = (lenf+lenr == 0)

  cons x (BD lenf f lenr r) = check (lenf+1) (x:f) lenr r

  head (BD lenf []     lenr r) = error "empty deque"
  head (BD lenf (x:f') lenr r) = x

  tail (BD lenf []     lenr r) = error "empty deque"
  tail (BD lenf (x:f') lenr r) = check (lenf-1) f' lenr r

  snoc (BD lenf f lenr r) x = check lenf f (lenr+1) (x:r)

  last (BD lenf f lenr []    ) = error "empty deque"
  last (BD lenf f lenr (x:r')) = x

  init (BD lenf f lenr []    ) = error "empty deque"
  init (BD lenf f lenr (x:r')) = check lenf f (lenr-1) r'
\end{code}
