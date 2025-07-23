/// https://adventofcode.com/2015/day/12
module Year2015Day12

open System
open System.Text.RegularExpressions

let numberRegex = Regex.numberRegex
let runPart1 (input: string) =
    numberRegex.Matches(input)
    |> Seq.sumBy (fun m -> Int32.Parse(m.Value))

let part2Regex = Regex("-?\\d+|[{}]|[\[\]]|\"red\"", RegexOptions.Compiled)
let runPart2 (input: string) =
    let typesArray = part2Regex.Matches(input)
    let rec calculate (idx: int) (ignoreRed: bool) (nullifyResult: bool) (currentSum: int): int * int =
        if idx >= typesArray.Count then
            idx, currentSum
        else
            let m = typesArray[idx].Value
            match m with
            | "["  | "{" ->
                let lastIndex, nextSum = calculate (idx + 1) (m = "[") nullifyResult 0
                calculate (lastIndex + 1) ignoreRed nullifyResult (nextSum + currentSum)
            | "]" | "}" ->
                //Return the current index. If we need to nullify the result, return 0 else return the current sum
                idx, if nullifyResult then 0 else currentSum
            | "\"red\"" ->
                //Set nullifyResult to true
                calculate (idx + 1) ignoreRed (if ignoreRed then nullifyResult else true) currentSum
            | x ->
                calculate (idx + 1) ignoreRed nullifyResult (currentSum + if nullifyResult then 0 else Int32.Parse(x))

    calculate 0 false false 0
    |> snd

let part1 (input: string[]) =
    runPart1 (input[0])

let part2 (input: string[]) =
    runPart2 (input[0])