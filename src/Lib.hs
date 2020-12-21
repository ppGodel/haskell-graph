module Lib
    ( someFunc
    ) where
import qualified Data.Map as Map

type Properties v = Map.Map String v
type Nodes k v = Map.Map k (Properties v)
type Edges k v = Map.Map k (Map.Map k (Properties v))

data Graph k v =
  Graph
    { nodes :: Nodes k v,
      edges :: Edges k v} deriving (Show, Eq)
order :: Graph k v -> Int
order graph = Map.size $ nodes graph

size :: Graph k v -> Int
size graph = Map.size $ edges graph

density :: Graph k v -> Float
density graph = fromIntegral (size graph) / fromIntegral (order graph * (order graph - 1))

lookupNode ::(Ord k) => Graph k v -> k -> Maybe (Properties v)
lookupNode graph key = Map.lookup key (nodes graph)

lookupStartEdges ::(Ord k) => Graph k v -> k -> Maybe (Map.Map k (Properties v))
lookupStartEdges graph key = Map.lookup key (edges graph)

lookupEdge ::(Ord k) => Graph k v -> k -> k -> Maybe (Properties v)
lookupEdge graph startNode endNode =  lookupStartEdges graph startNode >>= \eg -> Map.lookup endNode eg

addEdge :: (Ord k) => Graph k v -> k -> k -> Properties v -> Graph k v
addEdge graph startNode endNode properties = Graph { nodes = nodes graph
                                                   , edges = Map.unionWith Map.union (edges graph) (Map.fromList [(startNode, Map.fromList [(endNode, properties)])])
                                                   }
addNode :: (Ord k) => Graph k v -> k -> Properties v -> Graph k v
addNode graph nodeKey properties = Graph { nodes = Map.union (nodes graph) (Map.fromList [(nodeKey, properties)])
                                            , edges = edges graph
                                            }

-- search :: (Ord k) => Graph k v -> ([(Int, k)], [k]) -> k -> [k] -> ([k] -> k) -> Maybe ([(Int, k)], [k])
-- search graph state incumbent ks accessFn
--  | length ks == 0 = Just state
--  | otherwise = do {
--      edg <- lookupStartEdges graph incumbent
--      return search graph (fst state ++ fmap \x -> () Map.keys edg, snd state ++   )
--                    }
    --lookupStartEdges graph incumbent >>= \x -> (((fst state) + 1), ((snd state) ++ Map.keys x))

-- breathFirstSearch :: (Ord k) => Graph k v -> k -> [k] -> [(Int k)]
-- breathFirstSearch graph startNode [] =

myGraph :: Graph String Int
myGraph = Graph { nodes = Map.fromList [("a", Map.fromList [("x", 1), ("y", 1)]), ("b", Map.fromList [("x", 1), ("y", 2)] ), ("c", Map.fromList [("x", 3), ("y", 1)] )]
                , edges = Map.fromList [("a", Map.fromList [("b", Map.fromList [("c", 2), ("e", 1)]), ("c", Map.fromList [("c", 5), ("e",1)])])]
                }

someFunc :: IO ()
someFunc = putStrLn . show $ addEdge (addNode myGraph "d" (Map.fromList [("x", 4), ("y", 3)])) "b" "d" (Map.fromList [("c", 10), ("e", 1)])  -- ( Map.size (Map.fromList [( 1, 'a'), (2, 'b')]))
