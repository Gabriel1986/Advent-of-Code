/// https://adventofcode.com/2023/day/1
module Day1
open System.Text.RegularExpressions

let singleDigitRegex = Regex(@"\d", RegexOptions.Compiled)
let lastSingleDigitRegex = Regex(@"\d", RegexOptions.RightToLeft ||| RegexOptions.Compiled)

let part1 (input: string[]) =
    input
    |> Array.sumBy (fun line ->
        (line |> singleDigitRegex.Match |> fun x -> x.Index, line |> lastSingleDigitRegex.Match |> fun x -> x.Index)
        |> fun (firstIndex, lastIndex) -> int $"{line[firstIndex]}{line[lastIndex]}"
    )

let digitRegex = Regex(@"(\d|one|two|three|four|five|six|seven|eight|nine)", RegexOptions.Compiled)
let lastDigitRegex = Regex(@"(\d|one|two|three|four|five|six|seven|eight|nine)", RegexOptions.RightToLeft ||| RegexOptions.Compiled)

let part2 (input: string[]) =
    let dictionary=
        [
            "1", 1
            "2", 2
            "3", 3
            "4", 4
            "5", 5
            "6", 6
            "7", 7
            "8", 8
            "9", 9
            "one", 1
            "two", 2
            "three", 3
            "four", 4
            "five", 5
            "six", 6
            "seven", 7
            "eight", 8
            "nine", 9
        ]
        |> Map.ofList

    input
    |> Array.sumBy (fun line ->
        let firstNumber = line |> digitRegex.Match |> fun x -> dictionary[x.Value]
        let lastNumber = line |> lastDigitRegex.Match |> fun x -> dictionary[x.Value]
        firstNumber * 10 + lastNumber
    )