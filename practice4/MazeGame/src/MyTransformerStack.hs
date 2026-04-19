{-# LANGUAGE InstanceSigs #-}
module MyTransformerStack where

import Data.Functor.Identity

-- ReaderT copied from my practice file 
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

reader :: Monad m => (r -> a) -> ReaderT r m a
reader f = ReaderT $ return . f

instance Functor m => Functor (ReaderT r m) where 
    fmap :: (a->b) -> ReaderT r m a -> ReaderT r m b 
    fmap f x = ReaderT $ fmap f . runReaderT x 

instance Applicative m => Applicative (ReaderT r m) where 
    pure :: a -> ReaderT r m a 
    pure = ReaderT . const . pure 

    (<*>) :: ReaderT r m (a -> b) -> ReaderT r m a -> ReaderT r m b 
    f <*> v = ReaderT $ \e -> runReaderT f e <*> runReaderT v e 

instance Monad m => Monad (ReaderT r m) where 
    (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b 
    m >>= k = ReaderT $ \e -> do 
        v <- runReaderT m e 
        runReaderT (k v)e 

class MonadTrans t where 
    lift :: Monad m => m a -> t m a 

instance MonadTrans (ReaderT r) where 
    lift :: Monad m => m a -> ReaderT r m a 
    lift m = ReaderT $ \_ -> m 

ask :: Monad m => ReaderT r m r 
ask = ReaderT return 

asks :: Monad m => (r -> a) -> ReaderT  r m a 
asks f = ReaderT $ return . f 

local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local f x = ReaderT $ runReaderT x . f

-- writerT copied from my practice files 

newtype WriterT w m a = WriterT {runWriterT :: m (a, w)}
instance Functor m => Functor (WriterT w m) where 
    fmap f = WriterT . fmap update . runWriterT 
        where update ~(y, log) = (f y, log)

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
    pure :: a -> WriterT w m a
    pure x = WriterT $ pure (x, mempty)
    (<*>) :: WriterT w m (a -> b) -> WriterT w m a -> WriterT w m b
    f <*> v = WriterT $ liftA2 update (runWriterT f) (runWriterT v)
        where update ~(g,w) ~(x,w') = (g x, w `mappend` w')

instance (Monoid w, Monad m) => Monad (WriterT w m) where 
    m >>= k = WriterT $ do 
        ~(v, w) <- runWriterT m 
        ~(v', w') <- runWriterT (k v)
        return (v' , w <> w')

instance Monoid w => MonadTrans (WriterT w) where 
    lift :: Monad m => m a -> WriterT w m a 
    lift m = WriterT $ do 
        x <- m 
        return (x, mempty)

listen :: Monad m => WriterT w m a -> WriterT w m (a, w)
listen m = WriterT $ do 
    ~(a,w ) <- runWriterT m 
    return ((a,w), w) 

censor :: Monad m => (w->w) -> WriterT w m a -> WriterT w m a 
censor f m = WriterT $ do 
    ~(a,w ) <- runWriterT m 
    return (a , f w) 

tell :: Monad m => w -> WriterT w m () 
tell w = writer ((), w)
writer :: Monad m => (a,w) -> WriterT w m a
writer = WriterT . return

-- type Writer w a = WriterT w Identitiy a -- now I get it LOL 

--StateT : 

newtype MyStateT s m a = MyStateT { runMyStateT :: s -> m (a, s) }

instance Functor m => Functor (MyStateT s m) where
    fmap f st = MyStateT $ \s ->
        fmap (\(a, s') -> (f a, s')) (runMyStateT st s)

instance Monad m => Applicative (MyStateT s m) where 
    pure x = MyStateT $ \s -> return (x, s)

    f <*> v = MyStateT $ \s -> do 
        (g, s') <- runMyStateT f s 
        (x, s'') <- runMyStateT v s' 
        return (g x , s'')

instance Monad m => Monad (MyStateT s m) where 
    m >>= k = MyStateT $ \s -> do 
        (v, s') <- runMyStateT m s
        runMyStateT (k v) s' 

instance MonadTrans (MyStateT s) where 
    lift m = MyStateT $ \s -> do 
        x <- m 
        return (x, s)

get :: Monad m => MyStateT s m s
get      = MyStateT $ \s -> return (s, s)

put :: Monad m => s -> MyStateT s m ()
put  s = MyStateT $ \_ -> return ((), s)

modify :: Monad m => (s -> s) -> MyStateT s m ()
modify f = MyStateT $ \s -> return ((), f s) 

--tst1 :: (Int, Int)
tst1 = runIdentity $ runMyStateT (do { put 10; modify (*3); x <- get; return x }) 0 


-- game sta