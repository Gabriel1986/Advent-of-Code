/// https://adventofcode.com/2022/day/4
module Year2022Day4
open System

let part1 (input: string array) =
    input
    |> Array.sumBy (fun line ->
        let split = line.Split(',')
        let firstAssignment = split[0].Split('-')
        let secondAssignment = split[1].Split('-')

        let (minA, maxA) = (int firstAssignment[0], int firstAssignment[1])
        let (minB, maxB) = (int secondAssignment[0], int secondAssignment[1])

        let overlapping = (minA <= minB && maxA >= maxB) || (minB <= minA && maxB >= maxA)
        if overlapping then 1 else 0
    )

let part2 (input: string array) =
    input
    |> Array.sumBy (fun line ->
        let split = line.Split(',')
        let firstAssignment = split[0].Split('-')
        let secondAssignment = split[1].Split('-')

        let (minA, maxA) = (int firstAssignment[0], int firstAssignment[1])
        let (minB, maxB) = (int secondAssignment[0], int secondAssignment[1])

        let overlapping = (minA <= maxB && maxA >= minB) || (minB <= maxA && maxB >= minA)
        if overlapping then 1 else 0
    )