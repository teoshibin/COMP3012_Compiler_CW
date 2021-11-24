module StateC where

newtype ST st a = S (st -> (a,st))

app :: ST st a -> st -> (a,st)
app (S f) = f

instance Functor (ST st) where
    -- fmap :: (a -> b) -> ST st a -> ST st b
    fmap g sta = S(\s -> 
        let (x,s1) = app sta s 
        in (g x, s1))
    
instance Applicative (ST st) where
    pure x = S(\s ->(x,s))

    -- (<*>) (ST st (a -> b)) -> (ST st a)