/// https://adventofcode.com/2023/day/12
module Year2023Day12
open System.Collections.Generic

type Node =
    {
        I: int
        J: int
        DistanceTraveled: int
        DistanceToGoal: int
        Elevation: int
    }
    static member Create (i: int, j: int, previousNode: Node option, goal: int * int, heights: int[,]) =
        {
            I = i
            J = j
            DistanceTraveled = match previousNode with None -> 0 | Some x -> x.DistanceTraveled + 1
            DistanceToGoal =  abs (fst goal - i) + abs (snd goal - j)
            Elevation = heights[i, j]
        }
    member me.Heuristic = me.DistanceTraveled + me.DistanceToGoal - me.Elevation
    member me.Neighbours (goal: int * int) (heights: int[,]) (maxI: int, maxJ: int) =
        let createNode (i: int, j: int) =
            //Don't add the node if the elevation > current elevation + 1
            if heights[i,j] > me.Elevation + 1 then
                None
            else
                Some (Node.Create (i, j, Some me, goal, heights))

        [
            if me.I < maxI then createNode (me.I + 1, me.J)
            if me.J < maxJ then createNode (me.I, me.J + 1)
            if me.I > 0 then createNode (me.I - 1, me.J)
            if me.J > 0 then createNode (me.I, me.J - 1)
        ]
        |> List.choose id

let private parseHeightMap (input: string array) =
    let mutable start = (0, 0)
    let mutable goal = (0, 0)
    let heightMap = Array2D.init input.Length input[0].Length (fun i j ->
        let character = input[i][j]
        match character with
        | 'S' ->
            start <- (i, j)
            'a'
        | 'E' ->
            goal <- (i, j)
            'z'
        | otherCharacter ->
            otherCharacter
        |> fun character -> character - 'a'
        |> int
    )

    Node.Create(fst start, snd start, None, goal, heightMap),
    goal,
    heightMap

//Had to use mutable collections as using immutable collections made this algorithm too slow :(
let performShortestPathAlgorithm (maze: int[,], startNode: Node, goal: int * int) =
    let openNodes = new PriorityQueue<Node, int> ()
    let visitedNodes = new HashSet<int * int> ()
    let maxI = maze.GetUpperBound(0)
    let maxJ = maze.GetUpperBound(1)
    let mutable minNode = startNode

    while not (minNode.I = fst goal && minNode.J = snd goal) do
        visitedNodes.Add (minNode.I, minNode.J) |> ignore
        for neighbour in minNode.Neighbours goal maze (maxI, maxJ) do
            if (not (visitedNodes.Contains (neighbour.I, neighbour.J))) then
                openNodes.Enqueue(neighbour, neighbour.Heuristic)
        minNode <- openNodes.Dequeue()

    minNode.DistanceTraveled

let part1 (input) =
    let (start, goal, heights) = parseHeightMap(input)
    performShortestPathAlgorithm(heights, start, goal)

//Taking advantage of the unique lay-out of the input
//1) The shortest path is always either up or down and not right or left from the start
//2) When going further up than the minimum up, we won't get a shorter route
//3) When going further down than the minimum down, we won't get a shorter route
let part2 (input) =
    let (start, goal, heights) = parseHeightMap(input)
    let mutable minDistance = performShortestPathAlgorithm(heights, start, goal)

    let rec searchUp (i: int) =
        let localMinDistance = performShortestPathAlgorithm(heights, { start with I = start.I + i }, goal)
        if localMinDistance <= minDistance then
            minDistance <- localMinDistance
            searchUp (i + 1)
        else
            ()

    let rec searchDown (i: int) =
        let localMinDistance = performShortestPathAlgorithm(heights, { start with I = start.I - i }, goal)
        if localMinDistance <= minDistance then
            minDistance <- localMinDistance
            searchDown (i + 1)
        else
            ()

    searchUp (1)
    searchDown (1)
    minDistance