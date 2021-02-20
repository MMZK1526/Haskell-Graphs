{-# LANGUAGE FlexibleInstances #-}

module Utilities where

-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs.

import           Control.Applicative
import           Control.Monad.Trans.State
import           Data.Either (Either(..), isLeft, isRight)
import           Data.Foldable (forM_, toList)
import           Data.Maybe (isJust, isNothing)
import           Prelude hiding (length)

-- Require installation
import           Data.IntMap.Lazy as IM 
  (IntMap(..), delete, empty, insert, update, (!))
import           Data.Sequence as S (Seq(..), length, singleton, (<|), (><))

--------------------------------------------------------------------------------
-- Loop Break Controls
--------------------------------------------------------------------------------

-- A type class that can be interpreted as a state of breaking.
class (Flaggable a) where
  isBreaking :: a -> Bool

-- A data type that simulates breaking from a loop.
type Terminate a = Either a a

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

returnBreak, returnContinue :: Monad m => a -> m (Terminate a)
returnBreak    = return . Left
returnContinue = return . Right

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

-- loop_ iterates the terminable monadic action until it returns breakFlag, 
-- discarding the results wrapped in the Terminate.
loop_ :: (Monad m, Flaggable l) => m l -> m (Terminate ())
loop_ m 
  = m >>= \s -> breakWhen (isBreaking s) (loop_ m)

-- loop iterates the terminable monadic action until it returns breakFlag, 
-- but does not discard the results.
loop :: Monad m => a -> (a -> m (Terminate a)) -> m (Terminate a)
loop i f = do
  b <- f i
  if isBreaking b
    then return b
    else loop (information b) f


--------------------------------------------------------------------------------
-- Data Types
--------------------------------------------------------------------------------

-- Union Find
-- Contains a number of integer equivalence classes, each signified by a 
-- representative, and we can merge two equivalence classes together as well as 
-- check the representative of any given element.

data UnionFind = UF (IntMap Int) (IntMap (Seq Int))
  deriving (Show)

emptyUF :: UnionFind
emptyUF = UF IM.empty IM.empty

initUF :: [Int] -> UnionFind
initUF ns
  = execState (forM_ ns $ \n -> do
    UF im ims <- get
    put $ UF (insert n n im) (insert n (singleton n) ims)
  ) emptyUF

-- Find the representative of an element.
-- Pre: the element is in one of the equivalence classes.
getRep :: Int -> UnionFind -> Int
getRep e (UF im _)
  = im ! e

-- Check if two elements belong to the same equivalence class.
-- Pre: the elements are in the equivalent classes. 
equiv :: Int -> Int -> UnionFind -> Bool
equiv = (. getRep) . liftA2 (==) . getRep

-- Take union of two equivalence classes, choosing one of the representatives
-- as the new representative.
-- Pre: the elements are in different equivalent classes.
unionFind :: Int -> Int -> UnionFind -> UnionFind
unionFind i j uf@(UF im sets)
  = UF im' sets'
  where
    i' = getRep i uf
    j' = getRep j uf
    si = sets ! i'
    sj = sets ! j'
    replace s r 
      = forM_ s $ \e -> get >>= put . insert e r
    im' 
      | length si < length sj = execState (replace si j') im
      | otherwise             = execState (replace sj i') im
    sets'
      | length si < length sj = insert j (si >< sj) (delete i sets)
      | otherwise             = insert i (si >< sj) (delete j sets)


--------------------------------------------------------------------------------
-- Miscellaneous
--------------------------------------------------------------------------------

-- runWhenJust returns () when the input is Nothing, and applies the monadic
-- action when the input is a Just.
runWhenJust :: Monad m => Maybe a -> m b -> m ()
runWhenJust m f
  | isNothing m = return ()
  | otherwise   = f >> return ()

-- minMaybeOn returns the minimum if both arguments are Justs; Nothing if both
-- arguments are Nothing; the argument that is a Just if one of them is a Just.
-- If the two arguments are equal, return the first one.
minMaybeOn :: (Ord b) => (a -> b) -> Maybe a -> Maybe a -> Maybe a
minMaybeOn f ma mb
  | isJust (ma >> mb) = if liftA2 (<=) (f <$> ma) (f <$> mb) == Just True
     then ma 
     else mb
  | isJust ma         = ma
  | isJust mb         = mb
  | otherwise         = Nothing

-- maxMaybeOn returns the maximum if both arguments are Justs; Nothing if both
-- arguments are Nothing; the argument that is a Just if one of them is a Just.
-- If the two arguments are equal, return the first one.
maxMaybeOn :: (Ord b) => (a -> b) -> Maybe a -> Maybe a -> Maybe a
maxMaybeOn f ma mb
  | isJust (ma >> mb) = if liftA2 (>=) (f <$> ma) (f <$> mb) == Just True 
    then ma 
    else mb
  | isJust ma         = ma
  | isJust mb         = mb
  | otherwise         = Nothing
