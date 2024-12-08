/// https://adventofcode.com/2024/day/1
module Year2024Day1
open System

let private readLists (input: string[]) =
    (([], []), input)
    ||> Seq.fold (fun (left, right) next ->
        let split = next.Split(" ") |> Array.filter (String.IsNullOrWhiteSpace >> not)
        (int (split[0].Trim())::left), (int (split[1].Trim())::right))

let part1 (input: string[]) =
    input
    |> readLists
    |> fun (left, right) -> (left |> List.sort, right |> List.sort)
    ||> Seq.zip
    |> Seq.sumBy (fun (left, right) -> Math.Abs(left - right))

let part2 (input: string[]) =
    let countOccurences (numbers: int list) =
        (new Map<int, int>([]), numbers)
        ||> List.fold (fun acc next ->
            acc |> Map.change next (Option.defaultValue 0 >> (+) 1 >> Some)
        )

    input
    |> readLists
    |> fun (left, right) -> countOccurences left, countOccurences right
    |> fun (left, right) ->
        left
        |> Seq.sumBy (fun nextKey ->
            let numericValue = nextKey.Key
            let nbOccurencesLeft = nextKey.Value
            let nbOccurencesRight = right.TryFind numericValue |> Option.defaultValue 0
            numericValue * nbOccurencesLeft * nbOccurencesRight
        )