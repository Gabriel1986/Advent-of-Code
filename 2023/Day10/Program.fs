/// https://adventofcode.com/2023/day/10
module Year2023Day10
open FSharp.Collections

type Direction =
    | Up
    | Down
    | Left
    | Right

type MovementResult =
    | ``Dead end``
    | Finished
    | Moved of Direction * (int * int)

let private makeMove (x, y) = function
    | Up -> Moved (Up, (x - 1, y))
    | Down -> Moved (Down, (x + 1, y))
    | Left -> Moved (Left, (x, y - 1))
    | Right -> Moved (Right, (x, y + 1))

let private followPipe (pipes: string array) (direction: Direction) (currentNodeX, currentNodeY) =
    if currentNodeX < 0 || currentNodeX >= pipes.Length || currentNodeY < 0 || currentNodeY >= pipes[0].Length then
        ``Dead end``
    else
        let makeMove = makeMove (currentNodeX, currentNodeY)
        match direction, pipes[currentNodeX][currentNodeY] with
        | Down, '|' -> makeMove Down
        | Up, '|' -> makeMove Up
        | Right, '-' -> makeMove Right
        | Left, '-' -> makeMove Left
        | Down, 'L' -> makeMove Right
        | Left, 'L' -> makeMove Up
        | Down, 'J' -> makeMove Left
        | Right, 'J' -> makeMove Up
        | Right, '7' -> makeMove Down
        | Up, '7' -> makeMove Left
        | Up, 'F' -> makeMove Right
        | Left, 'F' -> makeMove Down
        | _, 'S' -> Finished
        | _ -> ``Dead end`` //Catches all illegal moves

let private parseInput (input: string array) =
    let mutable startingNode = (0, 0)
    input |> Array.iteri (fun lineIdx -> fun line ->
        line |> Seq.iteri (fun charIdx -> fun value ->
            if value = 'S' then startingNode <- (lineIdx, charIdx)
        ))
    (input, startingNode)

let private findLoop (pipes: string array) (startingNode: int * int) =
    let rec traverse (acc: (Direction * (int * int)) list) (previousDirection: Direction) (currentNode: int * int) =
        match followPipe pipes previousDirection currentNode with
        | Finished ->
            (previousDirection, currentNode)::acc
        | Moved (direction, nextNode) ->
            traverse ((previousDirection, currentNode)::acc) direction nextNode
        | ``Dead end`` ->
            List.empty

    seq { Up; Down; Left; Right }
    |> Seq.map (fun direction -> makeMove startingNode direction)
    |> Seq.map (function Moved (direction, nextNode) -> traverse List.empty direction nextNode | _ -> List.empty)
    |> Seq.find ((<>) List.empty)

let part1 (input) =
    parseInput input
    ||> findLoop
    |> List.length

let part2 (input) =
    let (pipes, startNode) = parseInput input
    let loop = findLoop pipes startNode |> List.rev
    let startSymbol =
        match (loop |> List.head |> fst), (loop |> List.last |> fst) with
        | Up, Up | Down, Down -> '|'
        | Left, Left | Right, Right -> '-'
        | Up, Left -> 'L'
        | Up, Right -> 'J'
        | Down, Left -> 'F'
        | Down, Right -> '7'
        | _ -> failwith "Invalid sequence"
    let loopSet = loop |> List.map snd |> Set.ofList
    //Clean up the array so it's easier to make decisions
    pipes
    |> Array.mapi (fun lineIdx line ->
        line
        |> String.mapi (fun charIdx -> fun value ->
            if value = '.' || loopSet.Contains ((lineIdx, charIdx)) then
                if value = 'S' then
                    //Replace S with the start symbol
                    startSymbol
                else
                    value
            else
                //Replace junk with ground
                '.')
        |> _.Replace("-", "").Replace("L7", "|").Replace("FJ", "|")
    )
    |> Array.sumBy (fun line ->
        ((0, 0), line)
        ||> Seq.fold (fun (pointsInLoop, nbLineCrosses) character ->
            if character = '.' then
                (nbLineCrosses % 2 + pointsInLoop, nbLineCrosses)
            elif character = '|' then
                (pointsInLoop, nbLineCrosses + 1)
            else
                (pointsInLoop, nbLineCrosses)
        )
        |> fst)