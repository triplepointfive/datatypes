{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module ChurchNumbers where

class Boolean a where
  tru :: a -> a -> a
  tru = \ t -> \ f -> t

  fls :: a -> a -> a
  fls = \ t -> \ f -> f

  test :: (a -> a -> a) -> a -> a -> a
  test = \ l -> \ m -> \ n -> l m n

instance (Boolean a) => (Boolean (a -> a -> a))

instance Boolean Bool

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

instance (Number a) => (Number ((a -> a) -> a -> a))

instance Number Integer

fix :: (a -> a) -> a
fix = \ f -> f (fix f)

fac = fix g
  where
    g = \ fct -> \ n -> if n == 0 then 1 else n * fct (n - 1)
