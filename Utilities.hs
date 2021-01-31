module Utilities where

-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs.

import           Control.Monad.Trans.State
import           Data.Foldable (toList)
import           Data.Maybe (isNothing)

-- A data type that simulates breaking from a loop.
data Terminate a = Terminate {
  isBreaking :: Bool,
  information :: a}

instance Functor Terminate where
  fmap f (Terminate bool info)
    = Terminate bool $ f info


-- Functions

terminate :: Terminate a -> Terminate a
terminate (Terminate _ a)
  = Terminate True a

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

-- forMTerminate_ maps elements of a foldable to a terminable monadic action, 
-- and breaks the iteration when the action indicates termination.
-- If action never terminates, in other words, if it is always in the form of 
-- m (Terminate False _), forMTerminate_ is similar to forM_.
forMTerminate_ :: (Foldable f, Monad m) 
  => f a 
  -> (a -> m (Terminate b)) 
  -> m (Terminate ())
forMTerminate_ xs f
  = forMT_ xs' f
  where
    xs' = toList xs
    forMT_ [] f
      = return $ Terminate False ()
    forMT_ (x : xs) f = do
      raw <- f x
      if isBreaking raw
        then return $ Terminate True ()
        else forMT_ xs f
