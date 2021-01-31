module Utilities where

-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs.

import           Control.Applicative
import           Control.Monad.Trans.State
import           Data.Either
import           Data.Foldable (toList)
import           Data.Maybe (isNothing)


-- A data type that simulates breaking from a loop:
type Terminate a = Either a a


--------------------------------------------------------------------------------
-- Breaking from loops
--------------------------------------------------------------------------------

terminate :: Terminate a -> Terminate a
terminate 
  = Left . information

isBreaking :: Terminate a -> Bool
isBreaking = isLeft

information :: Terminate a -> a
information (Left a)
  = a
information (Right a)
  = a

-- runUnlessBreak returns (Terminate ()) when the input indicates termination, 
-- and applies the state it does not terminate.
runUnlessBreak :: Terminate a -> State b (Terminate c) -> State b (Terminate ())
runUnlessBreak m f
  | isBreaking m = breakLoop
  | otherwise    = f >>= return . flag

-- forMBreak_ maps elements of a foldable to a terminable monadic action, 
-- and breaks the iteration when the action indicates termination.
-- If action never terminates, in other words, if it is always in the form of 
-- m (Terminate False _), forMBreak_ is similar to forM_.
forMBreak_ :: (Foldable f, Monad m) 
  => f a 
  -> (a -> m (Terminate b)) 
  -> m (Terminate ())
forMBreak_ xs f
  = forMB_ xs' f
  where
    xs' = toList xs
    forMB_ [] f
      = return $ Right ()
    forMB_ (x : xs) f = do
      raw <- f x
      if isBreaking raw
        then return $ Left ()
        else forMB_ xs f

breakLoop, continueLoop :: Monad m => m (Terminate ())
breakLoop    = return breakFlag
continueLoop = return continueFlag

breakFlag, continueFlag :: Terminate ()
breakFlag    = Left ()
continueFlag = Right ()

flag :: Terminate a -> Terminate ()
flag (Left _)
  = Left ()
flag (Right _)
  = Right ()

-- Break if the predicate is true.
breakUpon :: Monad m => Bool -> m (Terminate ())
breakUpon True
  = breakLoop
breakUpon _
  = continueLoop


--------------------------------------------------------------------------------
-- Miscellaneous
--------------------------------------------------------------------------------

-- runWhenJust returns () when the input is Nothing, and applies the state
-- when the input is a Just.
runWhenJust :: Maybe a -> State b () -> State b ()
runWhenJust m f
  | isNothing m = return ()
  | otherwise   = f
