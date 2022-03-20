{-# LANGUAGE NoImplicitPrelude #-}

import Prelude (Show, flip, print, ($), (*), (+), (.))

myList = Cons 1 $ Cons 2 $ Cons 3 $ Cons 4 Nil

myFuncs = Cons (* 2) $ Cons (+ 3) Nil

main = do
  print $ reverse myList
  print $ reversel myList
  print $ reverser myList
  print $ uncurry const (1, 2)

-- print $ bind (\item -> fmap (* item) myList) myList
-- print $ apply myFuncs myList
-- print $ apply (\input wrapped -> Cons wrapped (Cons (MyNewType input) Nil)) MyNewType 5
-- print $ getZipList (apply (ZipList myFuncs) (ZipList myList))

curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

id :: x -> x
id x = x

const :: x -> a -> x
const x _ = x

snd :: (x, y) -> y
snd = uncurry (flip const)

fst :: (x, y) -> x
fst = uncurry const

swap :: (x, y) -> (y, x)
swap = uncurry (flip (,))

class Functor m where
  fmap :: (a -> b) -> m a -> m b

class Functor m => Applicative m where
  pure :: a -> m a
  apply :: m (a -> b) -> m a -> m b

-- fn apply<A, B>(Self<FnOnce(A) -> B>, Self<A>) -> Self<B>

class Applicative m => Monad m where
  join :: m (m a) -> m a

-- bind :: (a -> m b) -> m a -> m b

-- join :: Monad m => m (m a) -> m a
-- join = bind id
bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join (fmap f m)

-- trait Functor {
--     fn fmap<A, B>(impl Fn(A) -> B, Self<A>) -> Self<B>;
-- }

data List a = Nil | Cons a (List a) deriving (Show)

reverse :: List a -> List a
reverse xs = go xs Nil
  where
    go Nil xs = xs
    go (Cons x xs) ys = go xs (Cons x ys)

reversel :: List a -> List a
reversel = foldl (flip Cons) Nil

reverser :: List a -> List a
reverser = foldr Cons Nil

foldr :: (item -> acc -> acc) -> acc -> List item -> acc
foldr f acc Nil = acc
foldr f acc (Cons x xs) = f x (foldr f acc xs)

-- x, y, z
--
-- f(f(f(seed, x), y), z)
-- f(x, f(y, f(z, seed)))

foldl :: (acc -> item -> acc) -> acc -> List item -> acc
foldl f acc Nil = acc
foldl f acc (Cons x xs) = foldl f (f acc x) xs

data Result e a = Err e | Ok a deriving (Show)

newtype ZipList a = ZipList {getZipList :: List a} deriving (Show)

-- newtype ZipList a = ZipList (List a)
-- getZipList :: ZipList a -> List a
-- getZipList (ZipList xs) = xs

newtype MyNewType a = MyNewType a deriving (Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Functor ZipList where
  fmap f (ZipList l) = ZipList (fmap f l)

instance Functor (Result e) where
  fmap f (Err e) = Err e
  fmap f (Ok t) = Ok (f t)

instance Functor ((->) r) where
  fmap f self = f . self

instance Applicative (Result e) where
  pure = Ok
  apply (Ok f) (Ok v) = Ok (f v)
  apply (Err e) _ = Err e
  apply _ (Err e) = Err e

-- instance Applicative List where
--   pure x = Cons x Nil
--   apply fs xs = bind (`fmap` xs) fs

-- instance Applicative ((->) r) where
--   pure = const
--   apply x y = bind (`fmap` y) x

-- instance Applicative ZipList where
--   pure x = ZipList (Cons x (pure x))
--   apply (ZipList Nil) _ = ZipList Nil
--   apply _ (ZipList Nil) = ZipList Nil
--   apply (ZipList (Cons f fs)) (ZipList (Cons x xs)) = ZipList (Cons (f x) (getZipList (apply (ZipList fs) (ZipList xs))))

-- instance Monad (Result e) where
--   bind f (Ok t) = f t
--   bind _f (Err e) = Err e

-- instance Monad List where
--   -- pure t = Cons t Nil
--   bind f Nil = Nil
--   bind f (Cons x xs) = concat (f x) (bind f xs)

-- concat :: List a -> List a -> List a
-- concat xs ys = foldr Cons ys xs

-- instance Monad ((->) r) where
--   bind f self x = f (self x) x

-- f :: (a -> (r -> b))
-- bind f (Cons x xs) = concat (f x) (bind f xs)

-- f :: a -> b
-- fmap f x

-- g :: a -> b -> c -> d
-- fmap g x y z ????
-- fmap g x :: m (b -> c -> d)
-- apply (fmap g x) y :: m (c -> d)
-- apply (apply (fmap g x) y) z :: m d
-- (<$>) = fmap
-- (<*>) = apply
-- g <$> x <*> y <*> z :: m d
-- f `fmap` x
-- (<$>) f x

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m