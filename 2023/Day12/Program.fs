/// https://adventofcode.com/2023/day/12
module Year2023Day12

open System.Collections.Generic

// Converts characters to elevations
let private charToHeight (c: char) : int =
    match c with
    | 'S' -> 0
    | 'E' -> 25
    | _ -> int c - int 'a'

// Parses the input map into coordinates and elevation matrix
let private parseHeightMap (input: string array) : (int * int) * (int * int) * int[,] =
    let rows = input.Length
    let cols = input.[0].Length
    let heights = Array2D.zeroCreate<int> rows cols

    // Initial start and goal positions
    let initialStart = -1, -1
    let initialGoal = -1, -1

    let (start, goal) =
        seq {
            for i in 0 .. rows - 1 do
                for j in 0 .. cols - 1 do
                    yield i, j, input.[i].[j]
        }
        |> Seq.fold (fun (s, g) (i, j, c) ->
            heights.[i, j] <- charToHeight c
            match c with
            | 'S' -> ((i, j), g)
            | 'E' -> (s, (i, j))
            | _ -> (s, g)
        ) (initialStart, initialGoal)

    start, goal, heights

// Dijkstra’s algorithm to find shortest path from start to goal with allowed climbing
let private dijkstra (start: int * int) (goal: int * int) (heights: int[,]) : int option =
    let maxI = heights.GetLength(0)
    let maxJ = heights.GetLength(1)
    let visited = Array2D.create<bool> maxI maxJ false
    let distances = Array2D.create<int option> maxI maxJ None
    let queue = new PriorityQueue<(int * int), int>() // ((i, j), dist)
    let mutable dist: int option = None

    let (si, sj) = start
    distances[si, sj] <- Some 0
    queue.Enqueue( (si, sj), 0)

    while queue.Count > 0 do
        match queue.TryDequeue() with
        | false, _, _ ->
            ()
        | true, (i, j), distanceSoFar ->
            if not visited[i, j] then
                visited[i, j] <- true
                if (i, j) = goal then
                    dist <- Some distanceSoFar
                let currentElevation = heights[i, j]
                for (di, dj) in [(-1, 0); (1, 0); (0, -1); (0, 1)] do
                    let ni, nj = i + di, j + dj
                    if ni >= 0 && ni < maxI && nj >= 0 && nj < maxJ then
                        let nextElevation = heights[ni, nj]
                        if nextElevation <= currentElevation + 1 then
                            let newDist = distanceSoFar + 1
                            match distances[ni, nj] with
                            | None ->
                                distances[ni, nj] <- Some newDist
                                queue.Enqueue((ni, nj), newDist)
                            | Some existing when newDist < existing ->
                                distances[ni, nj] <- Some newDist
                                queue.Enqueue((ni, nj), newDist)
                            | _ -> ()

    dist

// Reverse Dijkstra: compute distance from goal to all cells going "downhill"
let private reverseDijkstra (goal: int * int) (heights: int[,]) : Option<int>[,] =
    let maxI = heights.GetLength(0)
    let maxJ = heights.GetLength(1)
    let distances = Array2D.init maxI maxJ (fun _ _ -> None)
    let visited = Array2D.create maxI maxJ false
    let queue = PriorityQueue<(int * int), int>() // ((i, j), distance)

    let (gi, gj) = goal
    distances[gi, gj] <- Some 0
    queue.Enqueue((gi, gj), 0)

    while queue.Count > 0 do
        match queue.TryDequeue () with
        | false, _, _ ->
            ()
        | true, (i, j), dist ->
            if not visited[i, j] then
                visited[i, j] <- true
                let currentElevation = heights[i, j]
                for (di, dj) in [(-1, 0); (1, 0); (0, -1); (0, 1)] do
                    let ni, nj = i + di, j + dj
                    if ni >= 0 && ni < maxI && nj >= 0 && nj < maxJ then
                        let nextElevation = heights[ni, nj]
                        if nextElevation >= currentElevation - 1 then
                            let newDist = dist + 1
                            match distances[ni, nj] with
                            | None ->
                                distances[ni, nj] <- Some newDist
                                queue.Enqueue((ni, nj), newDist)
                            | Some existing when newDist < existing ->
                                distances[ni, nj] <- Some newDist
                                queue.Enqueue((ni, nj), newDist)
                            | _ -> ()

    distances

// Part 1: shortest path from 'S' to 'E' using regular Dijkstra
let part1 (input: string array) : int =
    let (start, goal, heights) = parseHeightMap input
    match dijkstra start goal heights with
    | Some d -> d
    | None -> failwith "No path found from start to goal"

// Part 2: shortest path from any 'a' (elevation 0) to 'E', using reverse Dijkstra
let part2 (input: string array) : int =
    let (_, goal, heights) = parseHeightMap input
    let distances = reverseDijkstra goal heights
    let maxI = heights.GetLength(0)
    let maxJ = heights.GetLength(1)

    seq {
        for i in 0 .. maxI - 1 do
            for j in 0 .. maxJ - 1 do
                if heights[i, j] = 0 then
                    match distances[i, j] with
                    | Some d -> yield d
                    | None -> ()
    }
    |> Seq.min