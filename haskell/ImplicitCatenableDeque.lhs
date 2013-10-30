\begin{code}
module ImplicitCatenableDeque (
  Sized(..),
  ImplicitCatDeque
  ) where

import Prelude hiding (head, tail, last, init, (++))
import CatenableDeque

class Sized d where
  size :: d a -> Int
  
data ImplicitCatDeque d a
    = Shallow (d a)
    | Deep (d a) (ImplicitCatDeque d (CmpdElem d a)) (d a)
                 (ImplicitCatDeque d (CmpdElem d a)) (d a)

data CmpdElem d a
    = Simple (d a)
    | Cmpd (d a) (ImplicitCatDeque d (CmpdElem d a)) (d a)

share f r = (init f, m, tail r)
  where m = cons (last f) (cons (head r) empty)

dappendL d1 d2 =
  if isEmpty d1 then d2
  else dappendL (init d1) (cons (last d1) d2)

dappendR d1 d2 =
  if isEmpty d2 then d1
  else dappendR (snoc d1 (head d2)) (tail d2)

replaceHead x (Shallow d) = Shallow (cons x (tail d))
replaceHead x (Deep f a mb r) = Deep (consx (tail f)) a m b r

instance (Deque d, Sized d) => Deque (ImplicitCatDeque d) where
  empty = Shallow empty
  isEmpty (Shallow d) = isEmpty d
  isEmpty _ = False

  cons x (Shallow d) = Shallow (cons x d)
  cons x (Deep f a m b r) = DEEP (consx f) a m b r

  head (Shallow d) = head d
  head (Deep f a m b r) = head f

  tail (Shallow d) = Shallow (tail d)
  tail (Deep f a m b r)
    | size f > 3 = Deep (tail f) a m b r
    | not (isEmpty a) =
      case head a of
        Simple d -> Deep f' (tail a) m b r
          where f' = dappendL (tail f) d
        Cmpd f' c' r' -> Deep f'' a'' m b r
          where f'' = dappendL (tail f) f'
                a'' = c' ++ replaceHead (Simple r') a
   | not (isEmpty b) =
      case head b of
        Simple d -> Deep f' empty d (tail b) r
          where f' = dappendL (tail f) m
        Cmpd f' c' r' -> Deep f'' a'' r' (tail b) r
          where f'' = dappendL (tail f) m
                a'' = cons (Simple f') c'
   | otherwise = Shallow (dappendL (tail f) m) ++ Shallow r

-- snoc, last, and init defined symmetrically...

instance (Deque d, Sized d) => CatenableDeque (ImplicitCatDeque d) where
  (Shallow d1) ++ (Shallow d2)
    | size d1 < 4 = Shallow (dappendL d1 d2)
    | size d2 < 4 = Shallow (dappendR d1 d2)
    | otherwise = let (f, m, r) = share d1 d2 in Deep f empty m empty r
  (Shallowd) ++ (Deep f a m b r)
    | size d1 < 4 = Deep (dappendL d f) a m b r
    | otherwise = Deep d (cons(Simple f) a) m b r
  (Deep f a m b r) ++ (Shallow d)
    | size d < 4 = Deep f a m b (dappendR r d)
    | otherwise = Deep f a m (snoc b (Simple r)) d
  (Deep f1 a1 m1 b1 r1) ++ (Deep f2 a2 m2 b2 r2) = Deep f1 a'1 m b'2 r2
    where (r'1, m, f'2) = share r1 f2
          a'1 = snoc a1 (Cmpd m1 b1 r'1)
          b'2 = cons (Cmpd f'2 a2 m2) b2
\end{code}
