-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs.

module SpanningTree where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.List (minimumBy)
import           Data.Maybe (fromJust)

-- May require installation
import           Data.IntMap.Lazy as IM 
  (IntMap(..), delete, empty, keys, insert, notMember, (!))
import           Data.Set as S 
  (Set(..), fromDescList, insert, notMember, member)

import           Graph
import           Utilities


--------------------------------------------------------------------------------
-- Prim's Algorithm
--------------------------------------------------------------------------------

-- Pre: The graph is undirected and the given node is in the graph.
-- primS :: (Graph a) => Int -> a -> Int -> [IntMap (Int, Int)]
primS x graph 
  = execState (forMBreak_ [2..(numNodes graph)] $ \_ -> do
    ((k, f), t) <- get
    let minN = minimumBy ((. (f !)) . compare . (f !)) (keys f) -- NULL CHECK!
    let minA = (fst $ f ! minN, minN)
    let k'   = S.insert minN k
    let f'   = execState (forM_ (neighbours minN graph) $ \s -> do
        fringe <- get
        continueWhen (S.member s k') $ do 
          let newW = fromJust $ weight (minN, s) graph
          if IM.notMember s fringe
          then put $ IM.insert s (minN, newW) fringe
          else put $ IM.insert s (minN, min newW (snd $ fringe ! s)) fringe
        ) $ delete minN f
    put ((k', f'), addUArcs [minA] t)
    ) ((fromDescList [x], initAdj), initGraph :: GraphList)
  where
    initAdj   = execState (forM_ (neighbours x graph) $ \s -> do
      fringe <- get
      put $ IM.insert s (x, fromJust (weight (x, s) graph)) fringe
      ) empty
    initGraph = initUGraph (nodes graph) [] 
