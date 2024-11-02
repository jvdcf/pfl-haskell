# TP1: Haskell Coursework

Programação Funcional e em Lógica (L.EIC024) 2024/2025  
Licenciatura em Engenharia Informática e Computação  
António Mário da Silva Marcos Florido (Regente e professor das aulas práticas)

**Grupo T02_G08**

- Guilherme Duarte Silva Matos (up202208755@up.pt)
- João Vítor da Costa Ferreira (up202208393@up.pt)

---

[TP1: Haskell Coursework](#tp1-haskell-coursework)
- [Contribuições do grupo](#contribuições-do-grupo)
- [Implementação de `shortestPath`](#implementação-de-shortestpath)
- [Implementação de `travelSales`](#implementação-de-travelsales)

---

## Contribuições do grupo

// TODO

> Identification of the group members, contribution of each member (in
> percentages adding up to 100%) and a brief description the tasks each one
> performed.

| Membro          | %   | Descrição das tarefas |
| --------------- | --- | --------------------- |
| Guilherme Matos | XX% | // TODO               |
| João Ferreira   | XX% | // TODO               |

## Implementação de `shortestPath`

The `shortestPath` function was implemented using the **Dijkstra's** algorithm to find the shortest path between two cities in a roadmap.
Below is the pseudocode of the algorithm used in the implementation:

```plaintext
shortestPath(roadmap, city1, city2):
    Mark all nodes as unvisited
    Initialize all the distances to infinity
    Set the distance to the starting node to 0
    Create a priority queue `q` and insert all the nodes with their distances
    While `q` is not empty:
        Pop the node `u` with the smallest distance from `q`
        Mark `u` as visited
        For each unvisited neighbor `v` of `u`:
            relax(roadmap, u, v)

relax(roadmap, u, v):
    distU = distance from the starting node to `u`
    distV = distance from the starting node to `v`
    weight = distance between `u` and `v`
    If distV >= distU + weight:
        Set the distance of `v` to distU + weight
        Set the previous node of `v` to `u`
```

The implementation uses two auxiliary data structures:

- A priority queue to store the node yet to be processed, ordered by the distance from the starting node. Implemented using a list of tuples with linear time search;

  > It is worth mentioning that the priority queue could be implemented using a binary heap with logarithmic access times to improve the time complexity of the algorithm, at the cost of simplicity.

- A list (with linear access time) of auxiliary variables for each city to store:
  - The distance from the starting node;
  - A list of the previous nodes to reconstruct the path;
  - If the node was already processed.

## Implementação de `travelSales`

// TODO

> A similar section to the previous one for the travelSales function.
