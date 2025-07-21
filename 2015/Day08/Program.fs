/// https://adventofcode.com/2015/day/8
module Year2015Day8

open System.Text.RegularExpressions
open Newtonsoft.Json

let part1 (input: string[]) =
    let totalLength = input |> Array.sumBy (fun x -> x.Length)
    //Unescape the string and remove the leading and trailing double quotes
    let totalEvaluatedLength = input |> Array.sumBy (fun x -> (Regex.Unescape x).Length - 2)

    totalLength - totalEvaluatedLength

let part2 (input: string[]) =
    let totalLength = input |> Array.sumBy (fun x -> x.Length)
    //This already escapes the string, no need to do this manually...
    let totalEscapedLength = input |> Array.sumBy (fun x -> (JsonConvert.ToString x).Length)

    totalEscapedLength - totalLength