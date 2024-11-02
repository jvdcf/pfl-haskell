import qualified Data.List
import qualified Data.Array
import qualified Data.Bits
import Debug.Trace ( trace )

{- TP1: Haskell Coursework
   Programação Funcional e em Lógica (L.EIC024) 2024/2025
   Licenciatura em Engenharia Informática e Computação
   António Mário da Silva Marcos Florido (Regente e professor das aulas práticas)

   Grupo T02_G08
   - Guilherme Duarte Silva Matos (up202208755@up.pt)
   - João Vítor da Costa Ferreira (up202208393@up.pt)
-}



-- Data structures ------------------------------------------------------------------------------

type City = String  -- Node of a graph
type Path = [City]  -- A list of nodes representing a path
type Distance = Int -- Weight of an edge

-- An adjacency list representation of the edges in a undirected and weighted graph
type RoadMap = [(City,City,Distance)]



-- Project convention ---------------------------------------------------------------------------

-- s  -> Source node/city
-- t  -> Sink or Destination node/city
-- rm -> RoadMap
-- c  -> City
-- d  -> Distance



-- Functions implemented ------------------------------------------------------------------------

{- Returns all the cities in the graph. -}
cities :: RoadMap -> [City]
cities rm = rmDupl ([ s | (s, _, _) <- rm] ++ [ t | (_, t, _) <- rm])
  where rmDupl = map head . Data.List.group . Data.List.sort


{- Returns a boolean indicating whether two cities are linked directly. -}
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent rm c1 c2 = length finds > 0
  where finds = [ True | (s, t, d) <- rm, s == c1, t == c2]


{- Returns a Just value with the distance between two cities connected directly, given two city names, and Nothing otherwise. -}
distance :: RoadMap -> City -> City -> Maybe Distance
distance rm c1 c2 | length finds > 0 = Just (head finds)
                  | otherwise = Nothing
                  where finds = [ d | (s, t, d) <- rm, s == c1, t == c2]


{- Returns the cities adjacent to a particular city (i.e. cities with a direct edge between them) and the respective distances to them. -}
adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent rm c = sinks ++ sources
  where sinks = [(t, d) | (s, t, d) <- rm, s == c]
        sources = [(s, d) | (s, t, d) <- rm, t == c]


{- Returns the sum of all individual distances in a path between two cities in a Just value, if all the consecutive pairs of cities are directly connected by roads. Otherwise, it returns a Nothing. -}
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance rm [] = Just 0
pathDistance rm [s] = Just 0
pathDistance rm (s:t:ps) =
  distance rm s t >>= \this ->
  pathDistance rm (t:ps) >>= \rest ->
  return (this + rest)


{- Returns the names of the cities with the highest number of roads connecting to them (i.e. the vertices with the highest degree). -}
rome :: RoadMap -> [City]
rome [] = []
rome rm = filter f (cities rm)
  where f c = degree c == maxDegree rm
        maxDegree rm = maximum [ degree c | c <- cities rm]
        degree c = length (adjacent rm c)


{- Returns a boolean indicating whether all the cities in the graph are connected in the roadmap (i.e., if every city is reachable from every other city). -}
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected [] = False
isStronglyConnected rm = length (dfsVisit rm startCity) == length (cities rm)
  where startCity = head (cities rm)
        dfsVisit rm c | cityNotBelongs = [c]
                      | otherwise      = c : dfsVisit subsetRoadMap nextCity
          where cityNotBelongs = c `notElem` cities rm
                subsetRoadMap  = filter (\(s, t, d) -> s /= c && t /= c) rm
                nextCity       = fst $ head $ adjacent rm c


{- Computes all shortest paths connecting the two cities given as input. Note that there may be more than one path with the same total distance. If there are no paths between the input cities, then return an empty list. Note that the (only) shortest path between a city c and itself is [c]. -}

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Nothing"

type State = [CityState]
data CityState = CityState {
  city :: City,
  dist :: Maybe Distance,
  prev :: [City],
  isVisited :: Bool
} deriving (Show)

sSearch :: State -> City -> CityState
sSearch [] _ = error "sSearch: City not found"
sSearch (s:r) c | city s == c = s
                | otherwise = sSearch r c

sUpdate :: State -> CityState -> State
sUpdate [] cs = error "sUpdate: City not found"
sUpdate (s:r) cs
  | city cs == city s && dist cs == dist s = CityState (city cs) (dist cs) (prev cs ++ prev s) (isVisited cs) : r
  | city cs == city s && dist cs < dist s = cs : r
  | city cs == city s && dist cs > dist s = s : r
  | otherwise = s : sUpdate r cs

-- Values inside the Heap
type UnvisitedCity = (Distance, City)

-- Priority Queue (TODO: Switch implementation to a binary heap)
type PriorityQueue = [UnvisitedCity]

pqPresent :: PriorityQueue -> City -> Bool
pqPresent [] _ = False
pqPresent (q:qs) c 
  | snd q == c = True
  | otherwise = pqPresent qs c

pqPush :: PriorityQueue -> UnvisitedCity -> PriorityQueue
pqPush queue city 
  | pqPresent queue (snd city) = error "pqPush: City already present"
  | otherwise = Data.List.sort (queue ++ [city])

pqTop :: PriorityQueue -> UnvisitedCity
pqTop queue
  | pqEmpty queue = error "pqTop: Empty queue"
  | otherwise = head queue

pqPop :: PriorityQueue -> PriorityQueue
pqPop queue
  | pqEmpty queue = error "pqPop: Empty queue"
  | otherwise = tail queue

pqEmpty :: PriorityQueue -> Bool
pqEmpty = null

pqSearch :: PriorityQueue -> City -> Maybe Distance
pqSearch [] c = trace ("pqSearch: City " ++ show c ++ "not found") Nothing
pqSearch (x:xs) c 
  | snd x == c = Just (fst x)
  | otherwise = pqSearch xs c

pqRemove :: PriorityQueue -> City -> PriorityQueue
pqRemove [] _ = error "pqRemove: City not found"
pqRemove (x:xs) c 
  | snd x == c = xs
  | otherwise = x : pqRemove xs c

pqUpdate :: PriorityQueue -> UnvisitedCity -> PriorityQueue
pqUpdate [] _ = error "pqUpdate: City not found"
pqUpdate (x:xs) (d, c) = pqPush (pqRemove (x:xs) c) (d, c)

dijkstra :: RoadMap -> City -> State -> PriorityQueue -> State
dijkstra rm dest state queue
  | pqEmpty queue = state
  -- | dest == snd (pqTop queue) = state
  | otherwise = 
    let
      (d, c) = pqTop queue
      poppedQueue = pqPop queue
      visitedState = sUpdate state (CityState c (Just d) [] True)
      adjacentCities = [fst a | a <- adjacent rm c, not (isVisited (sSearch visitedState (fst a)))]
      (newQueue, newState) = trace (show c ++ " adj: " ++ show adjacentCities) relax rm visitedState poppedQueue c adjacentCities
    in dijkstra rm dest newState newQueue

relax :: RoadMap -> State -> PriorityQueue -> City -> [City] -> (PriorityQueue, State)
relax _ state queue _ [] = trace "s" (queue, state)
relax rm state queue u (v:rest) 
  | distVS > newDist = trace "y" relax rm newState newQueue u rest
  | otherwise = trace "o" relax rm state queue u rest
  where
    us = sSearch state u
    vs = trace (show $ sSearch state v) sSearch state v
    distVS = case dist vs of
      Just d -> d
      Nothing -> maxBound
    newDist = fromJust (dist us) + fromJust (distance rm u v)
    newState = sUpdate state (CityState v (Just newDist) [u] False)
    newQueue = pqUpdate queue (newDist, v)

shortestPath :: RoadMap -> City -> City -> State
shortestPath rm s t = dijkstra rm t initState initQueue
  where initState = [CityState c (if c == s then Just 0 else Nothing) [] False | c <- cities rm]
        initQueue = foldl pqPush [] [(if c == s then 0 else maxBound, c) | c <- cities rm]
        


{- Given a roadmap, returns a solution of the Traveling Salesman Problem (TSP): visit each city exactly once and come back to the starting town in the route whose total distance is minimum. Any optimal TSP path will be accepted and the function only needs to return one of them, so the starting city (which is also the ending city) is left to be chosen by each group. If the graph does not have a TSP path, then return an empty list. -}
travelSales :: RoadMap -> Path
travelSales [] = []
travelSales rm = undefined -- TODO (Recommended to implement using dynamic programming)



-- Functions not applicable to our group of 2 people --------------------------------------------

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function



-- Some graphs for testing ----------------------------------------------------------------------

gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]

main :: IO()
main = undefined
