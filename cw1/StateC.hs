{- Compilers COMP3012
   15 Nov 2021
   Nicolai Kraus
-}

module StateC where


{- 
    STATE TRANSFORMER
-}

newtype ST st a = S (st -> (a,st))

app :: ST st a -> st -> (a,st)
app (S f) = f

instance Functor (ST st) where
    -- fmap :: (a -> b) -> ST st a -> ST st b
    fmap g sta = S(\s ->
        let (x,s1) = app sta s
        in (g x , s1))

instance Applicative (ST st) where
    pure x = S(\s -> (x,s))

    -- (<*>) :: (ST st (a -> b)) -> (ST st a)
    --                           -> (ST st b)
    stf <*> sta = S(\s ->
        let (f,s1) = app stf s
            (x,s2) = app sta s1
        in (f x , s2))

instance Monad (ST st) where
    return = pure

    -- (>>=) :: (ST st a) -> (a -> ST st b) -> ST st b
    sta >>= f = S(\s ->
        let (x,s1) = app sta s
            (y,s2) = app (f x) s1
        in (y,s2))

stUpdate :: st -> ST st ()
stUpdate s = S(\_ -> ((),s))

stGet :: ST st st
stGet = S(\s -> (s,s))

stRevise :: (st -> st) -> ST st ()
stRevise f = stGet >>= stUpdate . f


{- 
    TODO STATE IO
-}

newtype StateIO st a = SIO (st -> IO (a,st))

appIO :: StateIO st a -> st -> IO (a,st)
appIO (SIO st) x = st x

liftIO :: IO a -> StateIO st a
liftIO m = 
    SIO (\s -> do 
        x <- m
        return (x,s)
        )

instance Functor (StateIO st) where
    -- fmap :: (a -> b) -> ST st a -> ST st b
    fmap g st = 
        SIO (\s -> do 
            (x,s') <- appIO st s
            return (g x, s')
            )

instance Applicative (StateIO st) where
    pure x = SIO (\s -> return (x,s))

    -- (<*>) :: (ST st (a -> b)) -> (ST st a)
    --                           -> (ST st b)
    stf <*> sta = 
        SIO (\s -> do
            (f,s1) <- appIO stf s
            (x,s2) <- appIO sta s1
            return (f x , s2)
            )

instance Monad (StateIO st) where
    return = pure

    -- (>>=) :: (ST st a) -> (a -> ST st b) -> ST st b
    sta >>= f = 
        SIO (\s -> do
            (x,s1) <- appIO sta s
            (y,s2) <- appIO (f x) s1
            return (y,s2)
            )

stUpdateIO :: st -> StateIO st ()
stUpdateIO s = SIO (\_ -> return ((),s))

stGetIO :: StateIO st st
stGetIO = SIO (\s -> return (s,s))

stReviseIO :: (st -> st) -> StateIO st ()
stReviseIO f = stGetIO >>= stUpdateIO . f


{- 
    TODO STATET (generic STATE m)
-}
-- -- More general definition: If m is a monad, define
-- newtype StateT st m a = StTm (st -> m (a ,st))

-- instance Functor (StateT st m) where
--   fmap = undefined
-- -- and so on