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

terminate :: Terminate a -> Terminate a
terminate = Left . information

start :: Terminate a -> Terminate a
start = Right . information

isBreaking :: Terminate a -> Bool
isBreaking = isLeft

information :: Terminate a -> a
information (Left a)
  = a
information (Right a)
  = a

breakLoop, continueLoop :: Monad m => m (Terminate ())
breakLoop    = return breakFlag
continueLoop = return continueFlag

breakFlag, continueFlag :: Terminate ()
breakFlag    = Left ()
continueFlag = Right ()

flag :: Terminate a -> Terminate ()
flag (Left _)
  = Left ()
flag _
  = Right ()

-- breakloop if the predicate is true; otherwise continueloop
breakUpon :: Monad m => Bool -> m (Terminate ())
breakUpon True
  = breakLoop
breakUpon _
  = continueLoop

-- Runs the monadic action if the predicate is true; otherwise continueLoop.
continueWhen :: Monad m => Bool -> m (Terminate a) -> m (Terminate ())
continueWhen True _
  = continueLoop
continueWhen False m
  = m >>= return . flag

-- Runs the monadic action if the predicate is true; otherwise continueLoop.
breakWhen :: Monad m => Bool -> m (Terminate a) -> m (Terminate ())
breakWhen True _
  = breakLoop
breakWhen False m
  = m >>= return . flag

-- runUnlessBreak returns (breakFlag) when the input indicates termination, 
-- and applies the monadic action if it does not terminate.
runUnlessBreak :: Monad m => Terminate a -> m (Terminate b) -> m (Terminate ())
runUnlessBreak b m
  | isBreaking b = breakLoop
  | otherwise    = m >>= return . flag

-- forMBreak_ maps elements of a foldable to a terminable monadic action, 
-- and breaks the iteration when the action indicates termination.
-- The results wrapped in the Terminate is disgarded.
-- If the action never terminates, in other words, if it is always in the form 
-- of m (continueFlag), forMBreak_ is similar to forM_.
forMBreak_ :: (Foldable f, Monad m) 
  => f a 
  -> (a -> m (Terminate b)) 
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
doWhile_ :: Monad m => m (Terminate a) -> m (Terminate ())
doWhile_ m 
  = m >>= flip breakWhen m . isBreaking


--------------------------------------------------------------------------------
-- Miscellaneous
--------------------------------------------------------------------------------

-- runWhenJust returns () when the input is Nothing, and applies the monadic
-- action when the input is a Just.
runWhenJust :: Monad m => Maybe a -> m b -> m ()
runWhenJust m f
  | isNothing m = return ()
  | otherwise   = f >> return ()
