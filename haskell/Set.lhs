\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
module Set (Set(..)) where

class Set s a where
  empty  :: s a
  insert :: a -> s a -> s a
  member :: a -> s a -> Bool
\end{code}
