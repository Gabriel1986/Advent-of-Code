/// https://adventofcode.com/2015/day/9
module Year2015Day9

open System

let parseNode (input: string) =
    let parts = input.Split(" = ")
    let locationParts = parts[0].Split(" to ")
    (locationParts[0], locationParts[1]), int parts[1]

let rec permute (list: 'a list) : seq<'a list> = 
    seq {
        match list with
        | [] -> yield []
        | _ ->
            for i in 0 .. List.length list - 1 do
                let x = list.[i]
                // Exclude element at index i to get the rest of the list
                let rest = list.[..i-1] @ list.[i+1..]
                // Recursively get permutations of the rest
                for perm in permute rest do
                    yield x :: perm
    }


let calculateTotalDistance (dictionary: Map<(string * string), int>) (permutation: string list) =
    let getDistance (x: string) (y: string) =
        match dictionary |> Map.tryFind (x, y) with
        | Some value -> value
        | None -> dictionary[y, x]

    permutation
    |> List.windowed 2
    |> List.sumBy (fun permutation -> getDistance permutation[0] permutation[1])

let parseInput (input: string[]) =
    input
    |> Array.map parseNode
    |> Map.ofArray

let allDestinations (mapOfDestinations: Map<(string * string), int>) =
    mapOfDestinations
    |> Map.keys
    |> Seq.collect (fun (source, destination) -> [ source; destination ])
    |> Seq.distinct
    |> List.ofSeq

let part1 (input: string[]) =
    let mapOfDestinations = parseInput input

    (Int32.MaxValue, permute (allDestinations mapOfDestinations))
    ||> Seq.fold (fun minDistance next ->
        let totalDistance = calculateTotalDistance mapOfDestinations next
        if totalDistance < minDistance then totalDistance else minDistance)

let part2 (input: string[]) =
    let mapOfDestinations = parseInput input

    (Int32.MinValue, permute (allDestinations mapOfDestinations))
    ||> Seq.fold (fun maxDistance next ->
        let totalDistance = calculateTotalDistance mapOfDestinations next
        if totalDistance > maxDistance then totalDistance else maxDistance)