/// https://adventofcode.com/2015/day/9
module Year2015Day9

open System
open Helpers

let calculateTotalDistance (distanceMap: Map<(string * string), int>) (permutation: string list) =
    let getDistance (x: string) (y: string) =
        match distanceMap |> Map.tryFind (x, y) with
        | Some value -> value
        | None -> distanceMap[y, x]

    permutation
    |> List.windowed 2
    |> List.sumBy (fun permutation -> getDistance permutation[0] permutation[1])

let parseInput (input: string[]) =
    let parseNode (input: string) =
        let parts = input.Split(" = ")
        let locationParts = parts[0].Split(" to ")
        (locationParts[0], locationParts[1]), int parts[1]

    let nodes =
        input
        |> Array.map parseNode

    let destinations =
        nodes
        |> Array.collect (fun ((start, dest), _) -> [| start; dest |])
        |> Array.distinct
        |> Array.toList

    let mapOfDestinations =
        nodes
        |> Map.ofArray

    mapOfDestinations, destinations

let part1 (input: string[]) =
    let distanceMap, destinations = parseInput input

    (Int32.MaxValue, List.permute destinations)
    ||> Seq.fold (fun minDistance next ->
        let totalDistance = calculateTotalDistance distanceMap next
        if totalDistance < minDistance then totalDistance else minDistance)

let part2 (input: string[]) =
    let distanceMap, destinations = parseInput input

    (Int32.MinValue, List.permute destinations)
    ||> Seq.fold (fun maxDistance next ->
        let totalDistance = calculateTotalDistance distanceMap next
        if totalDistance > maxDistance then totalDistance else maxDistance)