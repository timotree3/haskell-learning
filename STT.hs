import Control.Monad (join)
import Control.Monad.ST (ST)

newtype STT s m a = STT {unSTT :: ST s (m a)}

instance (Functor m) => Functor (STT s m) where
  fmap f (STT m) = STT ((f <$>) <$> m)

instance (Applicative m) => Applicative (STT s m) where
  pure = STT . pure . pure
  (STT f) <*> (STT x) = STT ((<*>) <$> f <*> x)

instance (Monad m, Traversable m) => Monad (STT s m) where
  (STT m) >>= f = STT (join <$> (m >>= traverse (unSTT . f)))