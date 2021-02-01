{-# LANGUAGE FlexibleInstances #-}

module Utilities where

-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs.

import           Control.Applicative
import           Control.Monad.Trans.State
import           Data.Either (Either(..), isLeft, isRight)
import           Data.Foldable (toList)
import           Data.Maybe (isNothing)


--------------------------------------------------------------------------------
-- Loop Break Controls
--------------------------------------------------------------------------------

-- A data type that simulates breaking from a loop:
type Terminate a = Either a a

-- A type class that can be interpreted as a state of breaking
class (Flaggable a) where
  isBreaking :: a -> Bool

instance {-# OVERLAPPABLE #-} Flaggable (Either a b) where
  isBreaking = isLeft

instance {-# OVERLAPPABLE #-} Flaggable a where
  isBreaking = const False

terminate, start :: Terminate a -> Terminate a
terminate = Left . information
start     = Right . information

information :: Terminate a -> a
information (Left a)
  = a
information (Right a)
  = a

breakFlag, continueFlag :: Terminate ()
breakFlag    = Left ()
continueFlag = Right ()

breakLoop, continueLoop :: Monad m => m (Terminate ())
breakLoop    = return breakFlag
continueLoop = return continueFlag

flag :: Flaggable l => l -> Terminate ()
flag l
  | isBreaking l = Left ()
  | otherwise    = Right ()

-- breakloop if the predicate is true; otherwise continueloop
breakUpon :: Monad m => Bool -> m (Terminate ())
breakUpon True
  = breakLoop
breakUpon _
  = continueLoop

-- Runs the monadic action if the predicate is true; otherwise continueLoop.
continueWhen :: (Monad m, Flaggable l) => Bool -> m l -> m (Terminate ())
continueWhen True _
  = continueLoop
continueWhen _ m
  = m >>= return . flag

-- Runs the monadic action if the predicate is true; otherwise breakLoop.
breakWhen :: (Monad m, Flaggable l) => Bool -> m l -> m (Terminate ())
breakWhen True _
  = breakLoop
breakWhen _ m
  = m >>= return . flag

-- runUnlessBreak returns (breakFlag) when the input indicates termination, 
-- and applies the monadic action if it does not terminate.
runUnlessBreak :: (Monad m, Flaggable l1, Flaggable l2) 
  => l1 
  -> m l2 
  -> m (Terminate ())
runUnlessBreak = breakWhen . isBreaking

-- forMBreak_ maps elements of a foldable to a terminable monadic action, 
-- and breaks the iteration when the action indicates termination.
-- The results wrapped in the Terminate is disgarded.
-- If the action never terminates, in other words, if it is always in the form 
-- of m (continueFlag), forMBreak_ is similar to forM_.
forMBreak_ :: (Foldable f, Monad m, Flaggable l) 
  => f a 
  -> (a -> m l) 
  -> m (Terminate ())
forMBreak_ xs m
  = forMB_ xs' m
  where
    xs' = toList xs
    forMB_ [] m
      = continueLoop
    forMB_ (x : xs) m 
      = m x >>= flip breakWhen (forMB_ xs m) . isBreaking

-- doWhile iterates the terminable monadic action until it returns breakFlag, 
-- disgarding the results wrapped in the Terminate.
loop_ :: (Monad m, Flaggable l) => m l -> m (Terminate ())
loop_ m 
  = m >>= \s -> breakWhen (isBreaking s) (loop_ m)


--------------------------------------------------------------------------------
-- Miscellaneous
--------------------------------------------------------------------------------

-- runWhenJust returns () when the input is Nothing, and applies the monadic
-- action when the input is a Just.
runWhenJust :: Monad m => Maybe a -> m b -> m ()
runWhenJust m f
  | isNothing m = return ()
  | otherwise   = f >> return ()
