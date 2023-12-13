/// https://adventofcode.com/2022/day/5
module Year2022Day5
open System
open System.Text.RegularExpressions

let digitRegex = Regex("\d+")

let parseContainerStacksAndMovements (input: string array) =
    let indexOfEmpty = input |> Array.findIndex String.IsNullOrWhiteSpace
    let (containerConfig, movements) = input |> Array.splitAt (indexOfEmpty + 1)
    let nbContainerStacks = (containerConfig[0].Length + 1) / 4

    let emptyContainerStacks: char list list =
        Array.create nbContainerStacks []
        |> Array.toList

    let containerStacks =
        (emptyContainerStacks, containerConfig |> Array.take (indexOfEmpty - 1))
        ||> Array.fold (fun stacks line ->
            let containersInCurrentLine =
                line
                |> Seq.chunkBySize 4
                |> Seq.map (fun x -> if x[1] = ' ' then None else Some x[1])
                |> Seq.toList

            (stacks, containersInCurrentLine)
            ||> List.map2 (fun stack containerOpt ->
                match containerOpt with
                | Some container -> List.append stack [container]
                | None -> stack
            )
        )

    containerStacks, movements

let part1 (input) =
    parseContainerStacksAndMovements input
    ||> Array.fold (fun stacks movement ->
        let matches = digitRegex.Matches(movement)
        let nbContainersToMove = int matches[0].Value
        let fromContainerIndex = int matches[1].Value - 1
        let toContainerIndex = int matches[2].Value - 1

        let (containersToMove, remainder) = stacks[fromContainerIndex] |> List.splitAt nbContainersToMove

        stacks
        |> List.mapi (fun i stack ->
            if i = fromContainerIndex then
                remainder
            elif i = toContainerIndex then
                List.append (containersToMove |> List.rev) stack
            else
                stack
        )
    )
    |> List.choose List.tryHead
    |> List.toArray
    |> String

let part2 (input) =
    parseContainerStacksAndMovements input
    ||> Array.fold (fun stacks movement ->
        let matches = digitRegex.Matches(movement)
        let nbContainersToMove = int matches[0].Value
        let fromContainerIndex = int matches[1].Value - 1
        let toContainerIndex = int matches[2].Value - 1

        let (containersToMove, remainder) = stacks[fromContainerIndex] |> List.splitAt nbContainersToMove

        stacks
        |> List.mapi (fun i stack ->
            if i = fromContainerIndex then
                remainder
            elif i = toContainerIndex then
                List.append containersToMove stack
            else
                stack
        )
    )
    |> List.choose List.tryHead
    |> List.toArray
    |> String