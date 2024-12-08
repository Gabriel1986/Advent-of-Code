/// https://adventofcode.com/2024/day/2
module Year2024Day2
open System
open Helpers

let readLine (line: string): int[] =
    line.Split(" ")
    |> Array.map int


let getIndexOfFirstError (correctSignature: int -> int -> bool) (sequence: int[]) =
    ((sequence[0], None), sequence |> Array.indexed |> Array.skip 1)
    ||> Array.fold (fun left (idx, right) ->
        match left with
        | (_, Some idx) -> left
        | (left, _) ->
            if correctSignature left right && Math.BetweenInclusive 1 3 (Math.Abs (left - right)) then
                right, None
            else
                left, Some idx)
    |> snd

let part1 (input: string[]) =
    input
    |> Array.sumBy (fun line ->
        let sequence = readLine line
        let getIndexOfFirstError = getIndexOfFirstError (if sequence[0] > sequence[sequence.Length - 1] then (>) else (<))

        match getIndexOfFirstError sequence with
        | Some _ -> 0
        | None -> 1
    )

let part2 (input: string[]) =
    input
    |> Array.sumBy (fun line ->
        let sequence = readLine line
        let getIndexOfFirstError = getIndexOfFirstError (if sequence[0] > sequence[sequence.Length - 1] then (>) else (<))
        let hasError sequence = (getIndexOfFirstError sequence).IsSome

        match getIndexOfFirstError sequence with
        | Some idx when hasError (sequence |> Array.removeAt idx) || hasError (sequence |> Array.removeAt (idx - 1)) ->
            0
        | _ ->
            1
    )