import qualified Data.List
import qualified Data.Array
import qualified Data.Bits

{- TP1: Haskell Coursework
   Programação Funcional e em Lógica (L.EIC024) 2024/2025
   Licenciatura em Engenharia Informática e Computação
   António Mário da Silva Marcos Florido (Regente e professor das aulas práticas)

   Grupo T02_GXX
   - Guilherme Duarte Silva Matos (up202208755@up.pt)
   - João Vítor da Costa Ferreira (up202208393@up.pt)
 -}

-- Data structures -----------------------------------------

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)] -- Adjacency list 

-- Functions implemented -----------------------------------

cities :: RoadMap -> [City]
cities rm = rmDupl ([ s | (s, _, _) <- rm] ++ [ t | (_, t, _) <- rm])
    where rmDupl = map head . Data.List.group . Data.List.sort

areAdjacent :: RoadMap -> City -> City -> Bool
areAdjacent = undefined

distance :: RoadMap -> City -> City -> Maybe Distance
distance = undefined

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent = undefined

pathDistance :: RoadMap -> Path -> Maybe Distance
pathDistance = undefined

rome :: RoadMap -> [City]
rome = undefined

isStronglyConnected :: RoadMap -> Bool
isStronglyConnected = undefined

shortestPath :: RoadMap -> City -> City -> [Path]
shortestPath = undefined

travelSales :: RoadMap -> Path
travelSales = undefined

-- Functions not applicable to our group of 2 people -------

tspBruteForce :: RoadMap -> Path
tspBruteForce = undefined -- only for groups of 3 people; groups of 2 people: do not edit this function

-- Some graphs for testing ---------------------------------

gTest1 :: RoadMap
gTest1 = [("7","6",1),("8","2",2),("6","5",2),("0","1",4),("2","5",4),("8","6",6),("2","3",7),("7","8",7),("0","7",8),("1","2",8),("3","4",9),("5","4",10),("1","7",11),("3","5",14)]

gTest2 :: RoadMap
gTest2 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest3 :: RoadMap -- unconnected graph
gTest3 = [("0","1",4),("2","3",2)]
