{- Compilers COMP3012
   15 Nov 2021
   Nicolai Kraus
-}

module StateC where

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