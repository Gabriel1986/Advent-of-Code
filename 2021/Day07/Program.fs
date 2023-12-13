/// https://adventofcode.com/2021/day/7
module Year2021Day7
open System

let private parseInput (input: string array) =
    input[0].Split(",")
    |> Array.map int32

let part1 (input) =
    let inputArray = parseInput input
    let sorted = Array.sort inputArray
    let median = sorted[Math.Ceiling(float sorted.Length / 2.) |> int]

    inputArray
    |> Array.fold (fun fuelUsed next -> fuelUsed + Math.Abs(next - median)) 0

let part2 (input) =
    let inputArray = parseInput input

    let calculateMinDistance (input: int[], minValue: int) =
        let rec calculateMinDistance (input: int[], minDistance: int, currentValue: int) =
            let sumOfDifferences =
                input
                |> Array.sumBy (fun x ->
                    let n = Math.Abs (x - currentValue)
                    n * (n + 1) / 2)
            match minDistance < sumOfDifferences with
            | true -> minDistance
            | false -> calculateMinDistance (input, sumOfDifferences, currentValue + 1)

        calculateMinDistance (input, Int32.MaxValue, minValue)

    calculateMinDistance (inputArray, Array.min inputArray)