\begin{code}
module AltBinaryRandomAccessList (BinaryList) where

import Prelude hiding (head, tail, lookup)
import RandomAccessList

data BinaryList a
    = Nil
    | Zero  (BinaryList (a, a))
    | One a (BinaryList (a, a))

uncons:: BinaryList a -> (a, BinaryList a)
uncons Nil = error "empty list"
uncons (One x Nil) = (x, Nil)
uncons (One x ps)  = (x, Zero ps)
uncons (Zero ps)   = let ((x, y), ps') = uncons ps in (x, One y ps')

fupdate :: (a -> a) -> Int -> BinaryList a -> BinaryList a
fupdate f i Nil = error "bad subscript"
fupdate f 0 (One x ps) = One (f x) ps
fupdate f i (One x ps) = cons x (fupdate f (i-1) (Zero ps))
fupdate f i (Zero ps)  = Zero (fupdate f' (i `div` 2) ps)
  where f' (x,y) = if i `mod` 2 == 0 then (f x, y) else (x, f y)

instance RandomAccessList BinaryList where
  empty = Nil
  isEmpty Nil = True
  isEmpty _   = False

  cons x Nil = One x Nil
  cons x (Zero ps)  = One x ps
  cons x (One y ps) = Zero (cons (x, y) ps)

  head xs = fst (uncons xs)
  tail xs = snd (uncons xs)

  lookup i Nil = error "bad subscript"
  lookup 0 (One x ps) = x
  lookup i (One x ps) = lookup (i-1) (Zero ps)
  lookup i (Zero ps)  = if i `mod` 2 == 0 then x else y
    where (x, y) = lookup (i `div` 2) ps

  update i y xs = fupdate (\x -> y) i xs
\end{code}
