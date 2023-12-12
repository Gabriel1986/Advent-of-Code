/// https://adventofcode.com/2021/day/13
module Year2021Day13
open System

type FoldInstruction =
    | XAxis of int
    | YAxis of int

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some (int (s.Substring(p.Length).Trim()))
    else
        None

let private parseInput (input: string array) =
    let dots =
        input
        |> Seq.takeWhile (fun x -> x <> "")
        |> Seq.map (fun coordinateString -> coordinateString.Split(",") |> (fun split -> int split[0], int split[1]))
        |> Set.ofSeq

    let foldInstructions =
        input
        |> Seq.rev
        |> Seq.takeWhile (fun x -> x <> "")
        |> Seq.map (
            function
            | Prefix "fold along x=" x -> FoldInstruction.XAxis x
            | Prefix "fold along y=" y -> FoldInstruction.YAxis y
            | other -> failwithf "Failed to parse fold instricution %s" other
        )
        |> Seq.rev

    dots, foldInstructions

let pivot (a: string array array): string array array =
    let maxXIndex = a.Length
    let maxYIndex = a[0].Length

    [|
        for i in 0..maxYIndex-1 do [|
            for j in 0..maxXIndex-1 do
                a[j][i]
        |]
    |]

let prettyPrint (a: string array array) =
    seq {
        yield ""
        for i in 0..a.Length-1 do
            yield (a[i] |> String.joinWith "")
    }
    |> String.joinWith "\n"

let foldPaper (paper: string array array) (foldInstruction: FoldInstruction) =
    match foldInstruction with
    | FoldInstruction.XAxis xFold ->
        [|
            let maxXIndex = paper.Length - 1
            for x in 0..xFold-1 do
                [|
                    let maxYIndex = paper[x].Length - 1
                    for y in 0..maxYIndex do
                        if paper[x][y] = "#" || paper[maxXIndex-x][y] = "#" then
                            "#"
                        else
                            "."
                |]
        |]
    | FoldInstruction.YAxis yFold ->
        [|
            for x in 0..paper.Length - 1 do
                [|
                    let maxYIndex = paper[x].Length - 1
                    for y in 0..yFold-1 do
                        if paper[x][y] = "#" || paper[x][maxYIndex-y] = "#" then
                            "#"
                        else
                            "."
                |]
        |]

let part1 (input: string array) =
    let (dots, foldInstructions) = parseInput (input)
    let maxXIndex = foldInstructions |> Seq.choose (function XAxis xAxisFold -> Some (xAxisFold * 2) | YAxis _ -> None) |> Seq.max
    let maxYIndex = foldInstructions |> Seq.choose (function YAxis yAxisFold -> Some (yAxisFold * 2) | XAxis _ -> None) |> Seq.max

    let paper =
        [|
            for x in 0..maxXIndex do
                [|
                    for y in 0..maxYIndex do
                        if dots.Contains((x, y)) then
                            "#"
                        else
                            "."
                |]
        |]

    (paper, Seq.head foldInstructions)
    ||> foldPaper 
    |> Array.sumBy (Array.sumBy (function "#" -> 1 | other -> 0))

let part2 (input: string array) =
    let (dots, foldInstructions) = parseInput (input)
    let maxXIndex = foldInstructions |> Seq.choose (function XAxis xAxisFold -> Some (xAxisFold * 2) | YAxis _ -> None) |> Seq.max
    let maxYIndex = foldInstructions |> Seq.choose (function YAxis yAxisFold -> Some (yAxisFold * 2) | XAxis _ -> None) |> Seq.max

    let paper =
        [|
            for x in 0..maxXIndex do
                [|
                    for y in 0..maxYIndex do
                        if dots.Contains((x, y)) then
                            "#"
                        else
                            "."
                |]
        |]

    (paper, foldInstructions)
    ||> Seq.fold foldPaper
    |> pivot
    |> prettyPrint