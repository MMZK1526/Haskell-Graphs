module Utilities where

-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs.

import           Control.Applicative
import           Control.Monad.Trans.State
import           Data.Either
import           Data.Foldable (toList)
import           Data.Maybe (isNothing)


-- A data type that simulates breaking from a loop:
type Terminate a = Either a a


-- Functions:

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

-- runWhenJust returns () when the input is Nothing, and applies the state
-- when the input is a Just.
runWhenJust :: Maybe a -> State b () -> State b ()
runWhenJust m f
  | isNothing m = return ()
  | otherwise   = f

-- runUntilBreak returns () when the input indicates termination, 
-- and applies the state it does not terminate.
runUntilBreak :: Terminate a -> State b () -> State b ()
runUntilBreak m f
  | isBreaking m = return ()
  | otherwise    = f

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
breakLoop    = return (Left ())
continueLoop = return (Right ())
