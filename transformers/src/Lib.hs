module Lib
  ( STT,
    MaybeT,
    IOT,
    lift,
    runMaybeT,
    liftMaybe,
  )
where

import Control.Monad (join)
import Control.Monad.ST (ST)

type MaybeT m a = Transformer m Maybe a

runMaybeT :: MaybeT m a -> m (Maybe a)
runMaybeT = unTransformer

liftMaybe :: (Applicative m) => Maybe a -> MaybeT m a
liftMaybe = Transformer . pure

lift :: (Functor m) => m a -> MaybeT m a
lift = Transformer . (pure <$>)

type STT s m a = Transformer (ST s) m a

unSTT :: STT s m a -> ST s (m a)
unSTT = unTransformer

type IOT m a = Transformer IO m a

unIOT :: IOT m a -> IO (m a)
unIOT = unTransformer

newtype Transformer m n a = Transformer {unTransformer :: m (n a)}

fmap2 :: (Functor m, Functor n) => (a -> b) -> m (n a) -> m (n b)
fmap2 f m = (f <$>) <$> m

apply2 :: (Applicative m, Applicative n) => m (n (a -> b)) -> m (n a) -> m (n b)
apply2 f x = (<*>) <$> f <*> x

join2R :: (Monad m, Monad n, Traversable n) => m (n (m (n a))) -> m (n a)
join2R m = join <$> (join $ sequenceA <$> m)

join2L :: (Monad m, Traversable m, Monad n) => m (n (m (n a))) -> n (m a)
join2L m = join $ sequenceA <$> join <$> sequenceA m

bind2R :: (Monad m, Monad n, Traversable n) => (a -> m (n b)) -> m (n a) -> m (n b)
bind2R f = join2R . fmap2 f

instance (Functor m, Functor n) => Functor (Transformer m n) where
  fmap f = Transformer . fmap2 f . unTransformer

instance (Applicative m, Applicative n) => Applicative (Transformer m n) where
  pure = Transformer . pure . pure
  (<*>) (Transformer f) = Transformer . apply2 f . unTransformer

instance (Monad m, Monad n, Traversable n) => Monad (Transformer m n) where
  (Transformer m) >>= f = Transformer $ bind2R (unTransformer . f) m
