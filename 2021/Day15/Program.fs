/// https://adventofcode.com/2021/day/15
module Year2021Day15

open System
open System.Collections.Generic

/// Represents a position in the maze
[<Struct>]
type Position = { I: int; J: int }

/// Priority queue node with heuristic and total distance
[<Struct>]
type Node =
    val Position: Position
    val DistanceTraveled: int
    val Heuristic: int

    new(pos, dist, goal) =
        let heuristic = dist + abs (goal.I - pos.I) + abs (goal.J - pos.J)
        { Position = pos; DistanceTraveled = dist; Heuristic = heuristic }

let performShortestPath (maze: int[,]) =
    let rows, cols = maze.GetLength(0), maze.GetLength(1)
    let goal = { I = rows - 1; J = cols - 1 }
    let visited = Array2D.create rows cols Int32.MaxValue
    let queue = PriorityQueue<Node, int>()
    let mutable result: int option = None

    queue.Enqueue(Node({ I = 0; J = 0 }, 0, goal), 0)

    while result.IsNone && queue.Count > 0 do
        let current = queue.Dequeue()
        let i, j = current.Position.I, current.Position.J

        if current.Position = goal then
            result <- Some current.DistanceTraveled
        else
            if current.DistanceTraveled >= visited[i, j] then
                () // already visited with shorter distance
            else
                visited[i, j] <- current.DistanceTraveled

                for di, dj in [(-1,0); (1,0); (0,-1); (0,1)] do
                    let ni, nj = i + di, j + dj
                    if ni >= 0 && nj >= 0 && ni < rows && nj < cols then
                        let cost = current.DistanceTraveled + maze[ni, nj]
                        if cost < visited[ni, nj] then
                            let neighbor = Node({ I = ni; J = nj }, cost, goal)
                            queue.Enqueue(neighbor, neighbor.Heuristic)

    if result.IsNone then 
        failwith "Goal not reachable"
    else 
        result.Value

let parseMaze (input: string[]) =
    Array2D.init input.Length input[0].Length (fun i j -> int (string input.[i].[j]))

let part1 input =
    input
    |> parseMaze
    |> performShortestPath

let expand (maze: int[,]) =
    let height, width = maze.GetLength(0), maze.GetLength(1)
    Array2D.init (height * 5) (width * 5) (fun i j ->
        let orig = maze[i % height, j % width]
        let added = i / height + j / width
        let value = orig + added
        if value > 9 then value - 9 else value)

let part2 input =
    input 
    |> parseMaze 
    |> expand 
    |> performShortestPath
