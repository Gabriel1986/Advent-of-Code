/// https://adventofcode.com/2024/day/3
module Year2024Day3

open System.Text.RegularExpressions


let part1Regex = Regex(@"mul\(\d+,\d+\)")
let part2Regex = Regex(@"(mul\(\d+,\d+\))|(do\(\))|(don't\(\))")
let numberRegex = Regex("\d+")

let part1 (input: string[]): int =
    input
    |> String.joinWith "\n"
    |> part1Regex.Matches
    |> Seq.sumBy (fun part ->
        let numbers = numberRegex.Matches(part.Value)
        int numbers[0].Value * int numbers[1].Value)

let part2 (input: string[]): int =
    input
    |> String.joinWith "\n"
    |> part2Regex.Matches
    |> fun matches -> ((1, 0), matches)
    ||> Seq.fold (fun (multiply, sum) part ->
        if part.Value.StartsWith("mul") then
            let numbers = numberRegex.Matches(part.Value)
            let result = if multiply = 0 then 0 else int numbers[0].Value * int numbers[1].Value
            (multiply, result + sum)
        else
            ((if part.Value.StartsWith("don't") then 0 else 1), sum)
        )
    |> snd