/// https://adventofcode.com/2021/day/1
module Year2021Day1
open System

let private parseLines (lines: string array) =
    lines |> Array.map (Int32.Parse)

let part1 (input) =
    input
    |> parseLines
    |> Seq.pairwise
    |> Seq.sumBy (fun (predecessor, current) -> if current > predecessor then 1 else 0)

let part2 (input) =
    let input = parseLines input

    let slidingWindowSums =
        [
            for i in 0..(input.Length-3) do
                yield [ input[i]; input[i+1]; input[i+2] ] |> List.sum
        ]

    slidingWindowSums
    |> List.pairwise
    |> List.sumBy (fun (predecessor, current) -> if current > predecessor then 1 else 0)