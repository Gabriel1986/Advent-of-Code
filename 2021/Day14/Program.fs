/// https://adventofcode.com/2021/day/14
module Year2021Day14
open System

let private parseInput (input: string array) =
    let polymer = input[0]

    let transformations =
        input
        |> Seq.skip 2
        |> Seq.map (fun each -> each.Split(" -> ") |> (fun x -> x[0], x[1][0]))
        |> Map.ofSeq

    polymer, transformations

module String =
    let windowed (windowSize: int) (str: string) = [|
        for i in 0..str.Length - windowSize do
            str[i..i + windowSize - 1]
    |]

let part1 (input) =
    let (polymer, transformations) = parseInput (input)

    let nbIterations = 10
    let resultPolymer =
        seq { 1..nbIterations }
        |> Seq.fold (fun polymer _ ->
            polymer
            |> String.windowed 2
            |> Seq.map (fun x -> string transformations[x] + string x[1])
            |> String.joinWith ""
            |> (fun x -> string polymer[0] + x)
        ) polymer

    let characterCounts = resultPolymer |> Seq.countBy id

    let maxCharacterCount = characterCounts |> Seq.maxBy (fun (_, count) -> count) |> snd
    let minCharacterCount = characterCounts |> Seq.minBy (fun (_, count) -> count) |> snd
    maxCharacterCount - minCharacterCount

let part2 (input) =
    let (polymer, transformations) = parseInput (input)

    let nbIterations = 40

    let polymerMap =
        polymer
        |> String.windowed 2
        |> Seq.fold (fun polymerMap str -> polymerMap |> Map.change str (function Some x -> Some (x+1I) | None -> Some 1I)) Map.empty

    let countMap =
        polymer
        |> Seq.fold (fun countMap character -> countMap |> Map.change character (function Some x -> Some (x+1I) | None -> Some 1I)) Map.empty

    let _, updatedCountMap =
        seq { 1..nbIterations }
        |> Seq.fold (fun (accPoly: Map<string, bigint>, accCount: Map<char, bigint>) _ ->
            accPoly
            |> Seq.fold (fun (accPolyInner, accCountInner) kvp ->
                let transformed1 = string kvp.Key[0] + string transformations[kvp.Key]
                let transformed2 = string transformations[kvp.Key] + string kvp.Key[1]
                let nbOccurences = kvp.Value

                accPolyInner
                |> Map.change transformed1 (function Some x -> Some (x+nbOccurences) | None -> Some nbOccurences)
                |> Map.change transformed2 (function Some x -> Some (x+nbOccurences) | None -> Some nbOccurences)
                ,
                accCountInner
                |> Map.change transformations[kvp.Key] (function Some x -> Some (x+nbOccurences) | None -> Some nbOccurences)
            ) (Map.empty, accCount)
        ) (polymerMap, countMap)

    let maxCharacterCount = updatedCountMap |> Seq.maxBy (fun kvp -> kvp.Value) |> (fun x -> x.Value)
    let minCharacterCount = updatedCountMap |> Seq.minBy (fun kvp -> kvp.Value) |> (fun x -> x.Value)
    maxCharacterCount - minCharacterCount