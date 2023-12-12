/// https://adventofcode.com/2023/day/1
module Day1
open System
open System.Text.RegularExpressions

let part1 (input) =
    let digitRegex = System.Text.RegularExpressions.Regex(@"\d")
    let lastDigitRegex = System.Text.RegularExpressions.Regex(@"\d", RegexOptions.RightToLeft)

    input
    |> Array.sumBy (fun line ->
        (line |> digitRegex.Match |> fun x -> x.Index, line |> lastDigitRegex.Match |> fun x -> x.Index)
        |> fun (firstIndex, lastIndex) -> Int32.Parse($"{line[firstIndex]}{line[lastIndex]}")
    )

let part2 (input) =
    let digitRegex = System.Text.RegularExpressions.Regex(@"(\d|one|two|three|four|five|six|seven|eight|nine)")
    let lastDigitRegex = System.Text.RegularExpressions.Regex(@"(\d|one|two|three|four|five|six|seven|eight|nine)", RegexOptions.RightToLeft)

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