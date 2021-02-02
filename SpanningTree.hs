-- By Sorrowful T-Rex; https://github.com/sorrowfulT-Rex/Haskell-Graphs.

module SpanningTree where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.List (minimumBy)
import           Data.Maybe (fromJust)
import           Prelude hiding (null)

-- May require installation
import           Data.IntMap.Lazy as IM 
  (IntMap(..), delete, empty, keys, insert, notMember, null, (!))
import           Data.Set as S 
  (Set(..), fromDescList, insert, notMember, member, size)

import           Graph
import           Utilities


--------------------------------------------------------------------------------
-- Prim's Algorithm
--------------------------------------------------------------------------------

-- Pre: The graph is undirected and the given node is in the graph.
primS :: Graph a => a -> Maybe a
primS graph
  | sz == 0      = Nothing
  | size k == sz = Just t
  | otherwise    = Nothing
  where
    sz          = numNodes graph
    root        = head $ nodes graph
    initAdj     = execState (forM_ (neighbours root graph) $ \s -> do
      fringe <- get
      put $ IM.insert s (root, fromJust (weight (root, s) graph)) fringe
      ) empty
    ((k, _), t) = execState (loop_ $ do
      ((k, f), t) <- get
      breakWhen (null f) $ do
        let minN = minimumBy ((. (f !)) . compare . (f !)) (keys f)
        let minA = let (n, w) = f ! minN in ((n, minN), w)
        let adj  = neighbours minN graph
        let k'   = S.insert minN k
        let fS   = breakWhen (size k' == sz) $ forM_ adj $ \s -> do
            fringe <- get
            continueWhen (S.member s k') $ do 
              let newW = fromJust $ weight (minN, s) graph
              put $ if IM.notMember s fringe
                then IM.insert s (minN, newW) fringe
                else IM.insert s (minN, min newW (snd $ fringe ! s)) fringe
        put ((k', execState fS $ delete minN f), addUWArcs [minA] t)
        ) ((fromDescList [root], initAdj), initUGraph (nodes graph) [])
