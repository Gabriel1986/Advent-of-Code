module Year2015Day2
open System

let parseInput (input: string array) =
    input
    |> Array.map (fun each ->
        each.Split("x")
        |> Array.map Int32.Parse
        |> (function [| l; w; h |] ->  l, w, h | _ -> failwithf "Invalid dimensions"))

let part1 (input) =
    let calculateRequiredPackingPaperArea (l, w, h) =
        let smallestDimensions = [ l; w; h ] |> List.sort |> List.take 2
        2 * l * w + 2 * w * h + 2 * l * h + smallestDimensions[0] * smallestDimensions[1]

    parseInput input
    |> Seq.sumBy calculateRequiredPackingPaperArea

let part2 (input) =
    let calculateBowLength (l, w, h) =
        let smallestDimensions = [ l; w; h ] |> List.sort |> List.take 2
        smallestDimensions[0] * 2 + smallestDimensions[1] * 2 + l * w * h

    parseInput input
    |> Seq.sumBy calculateBowLength