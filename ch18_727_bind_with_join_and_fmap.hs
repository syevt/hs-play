import Control.Monad

andOne x = [x, 1]

bind :: Monad m => (a -> m b) -> m a -> m b
bind f a = join $ fmap f a
