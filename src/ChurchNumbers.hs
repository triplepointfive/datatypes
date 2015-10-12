{-# LANGUAGE RankNTypes #-}
module ChurchNumbers where

class Number a where
  c0, c1, c2, c3 :: (a -> a) -> a -> a
  c0 = \ s -> \ z -> z
  c1 = \ s -> \ z -> s z
  c2 = \ s -> \ z -> s (s z)
  c3 = \ s -> \ z -> s (s (s z))

  scc :: ((a -> a) -> a -> a) -> ((a -> a) -> a -> a)
  scc = \ n -> \ s -> \z -> n s (s z)

  plus :: ((a -> a) -> a -> a) -> ((a -> a) -> a -> a) -> (a -> a) -> a -> a
  plus = \ n -> \ m -> \ s -> \ z -> m s (n s z)

  times :: ((a -> a) -> a -> a) -> ((a -> a) -> a -> a) -> (a -> a) -> a -> a
  times = \ m -> \ n -> \ s -> \ z -> m (n s) z

instance Number Integer

fix :: (a -> a) -> a
fix = \ f -> f (fix f)

fac = fix g
  where
    g = \ fct -> \ n -> if n == 0 then 1 else n * fct (n - 1)
