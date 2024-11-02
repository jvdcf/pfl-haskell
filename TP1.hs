
import qualified Data.List
import qualified Data.Array
import qualified Data.Bits


{- TP1: Haskell Coursework
   Programação Funcional e em Lógica (L.EIC024) 2024/2025
   Licenciatura em Engenharia Informática e Computação
   António Mário da Silva Marcos Florido (Regente e professor das aulas práticas)

   Grupo T02_G08
   - Guilherme Duarte Silva Matos (up202208755@up.pt)
   - João Vítor da Costa Ferreira (up202208393@up.pt)
-}

-- Data structures ------------------------------------------------------------------------------

type City = String -- Node of a graph

type Path = [City] -- A list of nodes representing a path

type Distance = Int -- Weight of an edge

-- An adjacency list representation of the edges in a undirected and weighted graph
type RoadMap = [(City, City, Distance)]

-- Project convention ---------------------------------------------------------------------------

-- s  -> Source node/city
-- t  -> Sink or Destination node/city
-- rm -> RoadMap
-- c  -> City
-- d  -> Distance

-- Functions implemented ------------------------------------------------------------------------

{- Returns all the cities in the graph. -}
cities :: RoadMap -> [City]
cities rm = Data.List.nub $ Data.List.sort $ [s | (s, _, _) <- rm] ++ [t | (_, t, _) <- rm]

{- Returns a boolean indicating whether two cities are linked directly. -}
areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent rm c1 c2 = not $ null finds 
  where
    finds = [True | (s, t, d) <- rm, (s == c1 && t == c2) || (s == c2 && t == c1)]

{- Returns a Just value with the distance between two cities connected directly, given two city names, and Nothing otherwise. -}
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x : _) = Just x

distance :: RoadMap -> City -> City -> Maybe Distance
distance rm c1 c2 = maybeHead finds
  where
    finds = [d | (s, t, d) <- rm, (s == c1 && t == c2) || (s == c2 && t == c1)]

{- Returns the cities adjacent to a particular city (i.e. cities with a direct edge between them) and the respective distances to them. -}
adjacent :: RoadMap -> City -> [(City, Distance)]
adjacent rm c = sinks ++ sources
  where
    sinks = [(t, d) | (s, t, d) <- rm, s == c]
    sources = [(s, d) | (s, t, d) <- rm, t == c]

{- Returns the sum of all individual distances in a path between two cities in a Just value, if all the consecutive pairs of cities are directly connected by roads. Otherwise, it returns a Nothing. -}
pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance rm [] = Just 0
pathDistance rm [s] = Just 0
pathDistance rm (s : t : ps) =
  distance rm s t >>= \this ->
    pathDistance rm (t : ps) >>= \rest ->
      return (this + rest)

{- Returns the names of the cities with the highest number of roads connecting to them (i.e. the vertices with the highest degree). -}
rome :: RoadMap -> [City]
rome [] = []
rome rm = filter f (cities rm)
  where
    f c = degree c == maxDegree rm
    maxDegree rm = maximum [degree c | c <- cities rm]
    degree c = length (adjacent rm c)

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "fromJust: Attempted to unwrap a `Nothing` value."

{- Returns a boolean indicating whether all the cities in the graph are connected in the roadmap (i.e., if every city is reachable from every other city). -}
isStronglyConnected :: RoadMap -> Bool
isStronglyConnected [] = False
isStronglyConnected rm = fromJust $ startCity >>= \s -> Just $ length (dfsVisit rm s) == length (cities rm)
  where
    startCity = maybeHead (cities rm)
    dfsVisit rm c
      | cityNotBelongs = [c]
      | otherwise = c : dfsVisit subsetRoadMap nextCity
      where
        cityNotBelongs = c `notElem` cities rm
        subsetRoadMap = filter (\(s, t, d) -> s /= c && t /= c) rm
        nextCity = fromJust $ (maybeHead $ adjacent rm c) >>= \x -> Just $ fst x

{- Computes all shortest paths connecting the two cities given as input. Note that there may be more than one path with the same total distance. If there are no paths between the input cities, then return an empty list. Note that the (only) shortest path between a city c and itself is [c]. -}

-- Auxiliary representation of distance to allow for infinite values
data AuxDistance = Finite Distance | Infinite deriving (Eq, Show)

instance Ord AuxDistance where
  compare :: AuxDistance -> AuxDistance -> Ordering
  Infinite `compare` Infinite = EQ
  Infinite `compare` _ = GT
  _ `compare` Infinite = LT
  Finite d1 `compare` Finite d2 = d1 `compare` d2

instance Num AuxDistance where
  (+) :: AuxDistance -> AuxDistance -> AuxDistance
  Infinite + _ = Infinite
  _ + Infinite = Infinite
  Finite d1 + Finite d2 = Finite (d1 + d2)

  (*) :: AuxDistance -> AuxDistance -> AuxDistance
  Infinite * _ = Infinite
  _ * Infinite = Infinite
  Finite d1 * Finite d2 = Finite (d1 * d2)

  abs :: AuxDistance -> AuxDistance
  abs Infinite = Infinite
  abs (Finite d) = Finite (abs d)

  signum :: AuxDistance -> AuxDistance
  signum Infinite = 1
  signum (Finite d) = Finite (signum d)

  fromInteger :: Integer -> AuxDistance
  fromInteger = Finite . fromInteger

  negate :: AuxDistance -> AuxDistance
  negate Infinite = Infinite
  negate (Finite d) = Finite (negate d)

-- The state of the Dijkstra algorithm in a given step
type State = [CityState]

-- The state of a city in the Dijkstra algorithm
data CityState = CityState {
  city :: City,           -- The respective city
  dist :: AuxDistance,    -- Distance to the city from the source
  prev :: [City],         -- Previous nodes in the shortest path
  isVisited :: Bool       -- Whether the city has been analyzed or not in the algorithm
} deriving (Show)

-- Search for a given city in the state
sSearch :: State -> City -> CityState
sSearch [] _ = error "sSearch: City not found"
sSearch (s:r) c | city s == c = s
                | otherwise = sSearch r c

-- Return a new state with the city updated
sUpdate :: State -> CityState -> State
sUpdate [] cs = error "sUpdate: City not found"
sUpdate (s:r) cs
  | city cs == city s && dist cs == dist s = CityState (city cs) (dist cs) (prev cs ++ prev s) (isVisited cs) : r
  | city cs == city s && dist cs < dist s = cs : r
  | city cs == city s && dist cs > dist s = s : r
  | otherwise = s : sUpdate r cs

-- A city that has not been yet analyzed and its distance to the source
type UnvisitedCity = (AuxDistance, City)

-- Priority Queue implemented as a list of unvisited cities
type PriorityQueue = [UnvisitedCity]

-- Check if a city is inside the queue
pqPresent :: PriorityQueue -> City -> Bool
pqPresent [] _ = False
pqPresent (q:qs) c
  | snd q == c = True
  | otherwise = pqPresent qs c

-- Push a new city into the queue
pqPush :: PriorityQueue -> UnvisitedCity -> PriorityQueue
pqPush queue city
  | pqPresent queue (snd city) = error "pqPush: City already present"
  | otherwise = Data.List.sort (queue ++ [city])

-- Get the city with the smallest distance to the source
pqTop :: PriorityQueue -> UnvisitedCity
pqTop queue
  | pqEmpty queue = error "pqTop: Empty queue"
  | otherwise = head queue

-- Remove the city with the smallest distance to the source
pqPop :: PriorityQueue -> PriorityQueue
pqPop queue
  | pqEmpty queue = error "pqPop: Empty queue"
  | otherwise = tail queue

-- Check if the queue is empty
pqEmpty :: PriorityQueue -> Bool
pqEmpty = null

-- Search for a city in the queue
pqSearch :: PriorityQueue -> City -> Maybe UnvisitedCity
pqSearch [] c = Nothing
pqSearch (x:xs) c
  | snd x == c = Just x
  | otherwise = pqSearch xs c

-- Remove a specific city from the queue
pqRemove :: PriorityQueue -> City -> PriorityQueue
pqRemove [] _ = error "pqRemove: City not found"
pqRemove (x:xs) c
  | snd x == c = xs
  | otherwise = x : pqRemove xs c

-- Update the distance of a city already in the queue
pqUpdate :: PriorityQueue -> UnvisitedCity -> PriorityQueue
pqUpdate [] _ = error "pqUpdate: City not found"
pqUpdate (x:xs) (d, c) = pqPush (pqRemove (x:xs) c) (d, c)

{- Dijkstra algorithm (RoadMap, City destination, Initial state, Initial priority queue) -> Updated State
   While the queue is not empty:
    - Pop the city with the smallest distance to the source from the queue;
    - Mark the city as visited;
    - "Relax" all the unvisited adjacent edges. 
   *The initial state and initial priority queue must have the source city with distance 0 and all the other cities with infinite distance (as well as every city marked as unvisited and every previous lists empty).
-}
dijkstra :: RoadMap -> City -> State -> PriorityQueue -> State
dijkstra rm dest state queue
  | pqEmpty queue = state
  | otherwise =
    let
      (d, c) = pqTop queue
      poppedQueue = pqPop queue
      visitedState = sUpdate state (CityState c d [] True)
      adjacentCities = [fst a | a <- adjacent rm c, not (isVisited (sSearch visitedState (fst a)))]
      (newQueue, newState) = relax rm visitedState poppedQueue c adjacentCities
    in dijkstra rm dest newState newQueue

{- Relax operation in the Dijkstra algorithm (RoadMap, State, PriorityQueue, Source city, Adjacent cities) -> (Updated PriorityQueue, Updated State)
   Being:
    - u the city being analyzed;
    - v the adjacent city;
    - dist <city> the distance from source;
    - distance <city> <city> the direct distance between the two cities.
   If dist v > dist u + distance u v, then update dist v to the new distance and add u to the previous list of v.
-}
relax :: RoadMap -> State -> PriorityQueue -> City -> [City] -> (PriorityQueue, State)
relax _ state queue _ [] = (queue, state)
relax rm state queue u (v:rest)
  | distVS >= newDist = relax rm newState newQueue u rest
  | otherwise = relax rm state queue u rest
  where
    us = sSearch state u
    vs = sSearch state v
    distVS = dist vs
    newDist = case distance rm u v of
      Just d -> dist us + Finite d
      Nothing -> Infinite
    newState = sUpdate state (CityState v newDist [u] False)
    newQueue = pqUpdate queue (newDist, v)

{- Generate all shortest paths between two cities (Destination city, State, Accumulated path) -> [Paths]
   This auxiliary functions transforms the final state of the Dijkstra algorithm into a list of paths.
-}
generatePaths :: City -> State -> Path -> [Path]
generatePaths c state acc
  | dist cs == Infinite = []
  | dist cs == Finite 0 = [c:acc]
  | otherwise = concat [ generatePaths p state (c:acc) | p <- prev cs]
  where cs = sSearch state c

-- The function itself
shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath rm s t = generatePaths t (dijkstra rm t initState initQueue) []
  where
    initState = [CityState c (if c == s then 0 else Infinite) [] False | c <- cities rm]
    initQueue = foldl pqPush [] [(if c == s then 0 else Infinite, c) | c <- cities rm]


-----------------------------------------------
{- Given a roadmap, returns a solution of the Traveling Salesman Problem (TSP): visit each city exactly once and come back to the starting town in the route whose total distance is minimum. Any optimal TSP path will be accepted and the function only needs to return one of them, so the starting city (which is also the ending city) is left to be chosen by each group. If the graph does not have a TSP path, then return an empty list. -}

type DistanceMatrix = Data.Array.Array (Int, Int) Distance

type DynamicProg = Data.Array.Array (Int, Int) Int

cityIndexOf :: RoadMap -> City -> Int
cityIndexOf rm c = case lookup c (zip (cities rm) [0 ..]) of
  Just idx -> idx
  Nothing -> error "City not found"

setToList :: (Data.Bits.Bits t1, Num t2, Integral t1) => t1 -> [t2]
setToList s = s2l s 0
  where
    s2l 0 _ = []
    s2l n i
      | odd n = i : s2l (n `div` 2) (i + 1)
      | otherwise = s2l (n `div` 2) (i + 1)

minimum' :: (Ord a) => [Maybe a] -> Maybe a
minimum' [] = Nothing
minimum' l = minimum l

table :: (Data.Bits.Bits a, Integral a) => RoadMap -> Int -> a -> Path -> Maybe (Distance, Path)
table rm i s p
  | s == Data.Bits.zeroBits = (distance rm (cityInt endCity) (cityInt i)) >>= \x -> Just (x, p ++ [cityInt endCity])
  | otherwise = minimum' $ filter (/= Nothing) [weight j >>= \w -> prevCell j >>= \c -> Just (w + fst c, snd c) | j <- setToList s]
  where
    endCity = length (cities rm) - 1
    weight j = (distance rm (cityInt i) (cityInt j))
    prevCell j = table rm j (Data.Bits.clearBit s j) (p ++ [cityInt j])
    cityInt i = cities rm !! i

travelSales :: RoadMap -> Path
travelSales [] = []
travelSales rm = case table rm (cityCount - 1) mask path of
                    Just (dist, p) -> p
                    Nothing -> []
  where
    cityCount = length $ cities rm
    mask = (Data.Bits.shiftL 1 (cityCount - 1)) - 1 :: Int
    path = [cities rm !! (cityCount - 1)]

-----------------------------------------------

-- Functions not applicable to our group of 2 people --------------------------------------------

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs for testing ----------------------------------------------------------------------

gTest1 :: RoadMap
gTest1 = [("7", "6", 1), ("8", "2", 2), ("6", "5", 2), ("0", "1", 4), ("2", "5", 4), ("8", "6", 6), ("2", "3", 7), ("7", "8", 7), ("0", "7", 8), ("1", "2", 8), ("3", "4", 9), ("5", "4", 10), ("1", "7", 11), ("3", "5", 14)]

gTest2 :: RoadMap
gTest2 = [("0", "1", 10), ("0", "2", 15), ("0", "3", 20), ("1", "2", 35), ("1", "3", 25), ("2", "3", 30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0", "1", 4), ("2", "3", 2)]


gTemp1 :: RoadMap -- various shortest paths between 0 and 3
gTemp1 = [("0","1",5),("0","2",5),("0","3",10),("1","2",99),("1","3",5),("2","3",5)]

main :: IO()
main = undefined
