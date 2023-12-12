/// https://adventofcode.com/2021/day/6
module Year2021Day6
open System

let private parseInput (input: string array) =
    input[0].Split(",")
    |> Array.map int32

///Doesn't scale.
let part1 (input) =
    let evolve (fish: int array, nbDays: int) =
        let rec evolve (fish: int array, currentDay: int) =
            if currentDay = nbDays then
                fish.Length
            else
                evolve (fish |> Array.collect (function 0 -> [| 6; 8 |] | other -> [| other - 1 |]), currentDay + 1)
        evolve (fish, 0)

    evolve (parseInput input, 80)
    
let part2 (input) =
    let input = parseInput input

    let initialGeneration: bigint array = [|
        for i = 0 to 8 do yield input |> Array.sumBy (fun x -> if x = i then bigint 1 else bigint 0)
    |]

    let evolve (generation: bigint array) =
        [|
            generation[1]
            generation[2]
            generation[3]
            generation[4]
            generation[5]
            generation[6]
            generation[7] + generation[0]
            generation[8]
            generation[0]
        |]

    let nbDays = 256

    seq { 1..nbDays }
    |> Seq.fold (fun generation _ -> evolve generation) initialGeneration
    |> Array.sum
