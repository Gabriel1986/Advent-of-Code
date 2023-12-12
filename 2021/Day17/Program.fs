/// https://adventofcode.com/2021/day/17
module Year2021Day17
open System

type Range =
    {
        LowerBound: int
        UpperBound: int
    }
    member me.Contains (number: int) =
        me.LowerBound <= number && me.UpperBound >= number

let private parseInput (input: string array) =
    let splitInTwo = function [| x; y |] -> x, y | _ -> failwithf "Unexpected input"
    let x, y = input[0].Replace("target area: ", "").Split(", ") |> splitInTwo
    let xRange = x.Replace("x=", "").Split("..") |> Array.map int |> Array.sort |> splitInTwo |> (fun (x1, x2) -> { LowerBound = x1; UpperBound = x2 })
    let yRange = y.Replace("y=", "").Split("..") |> Array.map int |> Array.sort |> splitInTwo |> (fun (y1, y2) -> { LowerBound = y1; UpperBound = y2 })
    xRange, yRange

let part1 (input) =
    let _, yTargetRange = parseInput (input)
    let maxY = -yTargetRange.LowerBound - 1

    maxY * (maxY + 1) / 2

let part2 (input) =
    let xTargetRange, yTargetRange = parseInput (input)

    let minX = seq { 1..Int32.MaxValue } |> Seq.find (fun x -> xTargetRange.Contains(x * (x + 1) / 2))
    let maxX = xTargetRange.UpperBound
    let minY = yTargetRange.LowerBound
    let maxY = -yTargetRange.LowerBound - 1

    let hasMatchingStep (x, y) =
        let rec increaseStep (currentX, sumX, currentY, sumY) =
            if xTargetRange.Contains sumX && yTargetRange.Contains sumY then
                true
            elif sumX > xTargetRange.UpperBound || sumY < yTargetRange.LowerBound then
                false
            else
                let newXSpeed = Math.Max(0, currentX - 1)
                let newXSum = newXSpeed + sumX
                let newYSpeed = currentY - 1
                let newYSum = newYSpeed + sumY
                increaseStep (newXSpeed, newXSum, newYSpeed, newYSum)

        increaseStep (x, x, y, y)

    //Brute forcing: since we have minX, maxX, minY and maxY we only have a limited amount of options to check
    [
        for x in minX..maxX do
            for y in minY..maxY do
                if hasMatchingStep (x, y) then (x, y) else ()
    ]
    |> List.length