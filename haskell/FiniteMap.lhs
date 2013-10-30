\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
module FiniteMap (FiniteMap(..)) where

class FiniteMap m k where
  empty  :: m k a
  bind   :: k -> a -> m k a -> m k a
  lookup :: k -> m k a -> Maybe a
\end{code}
