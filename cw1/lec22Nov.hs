{-
Compilers Course (COMP3012), 2021
  Nicolai Kraus / Venanzio Capretta

relevant:
Venanzio's videos 10c, 11a, 11b, 12a
-}

import StateTest

-- # Clarification on TAM instructions

type MTInt = Int
type Address = Int
type LName = String

data TAMInstr =
    LOADL MTInt   -- put some *value* on top of stack
  | LOAD Address  -- copy content of memory cell with given *address* to top of stack
  | STORE Address -- pop (i.e. remove) top of stack and write it to memory cell with given address
  | PUTINT        -- pop top of stack and print it
  | JUMPIFZ LName -- pop top of stack, if it's 0 then jump to label
  | JUMP LName    -- unconditional jump
  | HALT          -- stop execution
  -- and so on
  deriving (Eq,Show)

-- # TAM States

type TAMPrgm = [TAMInstr]
type Stack = [MTInt]
type Counter = Int

data TAMState = TAMState {
  tsCode :: TAMPrgm ,
  tsCounter :: Counter,
  tsStack :: Stack
}
  deriving (Eq,Show)

tsPush :: MTInt -> TAMState -> TAMState
tsPush n ts = ts {tsStack = n : (tsStack ts)}

tsPop :: TAMState -> (MTInt,TAMState)
tsPop ts =
  let stack = tsStack ts
  in (head stack, ts {tsStack = tail stack})

-- We could/should use the State (Transformer) Monad!

type TAMSt a = ST TAMState a

tsPop1 :: TAMSt MTInt
tsPop1 = undefined

-- here: continue without TAMSt

-- find a given label:
lCounter :: LName -> TAMState -> Counter
-- or LName -> TATPrgm -> Counter
lCounter = undefined

-- # Stack reading/writing

tsRead :: Address -> TAMState -> MTInt
tsRead = undefined

-- CAVEAT: address 0 is bottom of stack, NOT head of the list

tsWrite :: Address -> MTInt -> TAMState -> TAMState
tsWrite = undefined

tsSetCounter :: Counter -> TAMState -> TAMState
tsSetCounter = undefined

-- # Executing a TAM program

execute :: TAMInstr -> TAMState -> TAMState
execute (JUMP l) ts =
  tsSetCounter (lCounter l ts) ts
execute (STORE a) ts =
  let  (x,ts1) = tsPop ts
       ts2 = tsWrite a x ts1
       ts3 = undefined -- increase counter by one
  in ts3

-- If we use TAMSt, we would do:

execute1 :: TAMInstr -> TAMSt ()
execute1 HALT = return ()
-- and so on (need auxiliary functions)

-- Problem:
execute1 PUTINT = undefined
-- this needs IO !

-- # Adding IO to states

newtype StateIO st a = StT (st -> IO (a,st))

-- StateIO is a monad, and all functions for State can be adapted to StateIO !

instance Functor (StateIO st) where
  fmap = undefined
-- and so on.

-- More general definition: If m is a monad, define
newtype StateT st m a = StTm (st -> m (a ,st))

instance Functor (StateT st m) where
  fmap = undefined
-- and so on
