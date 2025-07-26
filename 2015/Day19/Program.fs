/// https://adventofcode.com/2015/day/19
module Year2015Day19

open System
open System.Text.RegularExpressions
open System.Collections.Generic

let parseInput (input: string[]): (string * string)[] * string =
    let conversions =
        input
        |> Array.takeWhile (String.IsNullOrWhiteSpace >> not)
        |> Array.map (fun str ->
            let split = str.Split(" => ")
            split[0], split[1])

    let molecule =
        input[input.Length - 1]

    conversions, molecule

let runPart1 (conversions: (string * string)[], molecule: string) =
    let alreadySeem = HashSet<string>()
    for (fromString, toString) in conversions do
        let matches = Regex.Matches(molecule, fromString)
        for idx in matches |> Seq.map (fun m -> m.Index) do
            alreadySeem.Add(molecule.Substring(0, idx) + toString + molecule.Substring(idx + fromString.Length)) |> ignore

    alreadySeem.Count

let part1 (input: string[]) =
    parseInput input
    |> runPart1

//Greedy replacements => only works because of the nature of the puzzle input:
//The puzzle’s replacements define a well-structured, hierarchical, non-ambiguous rewriting system (no overlaps, no cycles).
let runPart2 (conversions: (string * string)[], molecule: string) =
    //Reverse then sort by descending length:
    //Replacing large strings first gets us to the result faster
    let reversedConversions =
        conversions
        |> Array.map (fun (f, t) -> (t, f))
        |> Array.sortByDescending (fun (toStr, _) -> toStr.Length)

    let rec greedyReduce currentMolecule steps =
        if currentMolecule = "e" then
            steps
        else
            // Try to find a replacement
            let rec tryReplace index =
                if index >= Array.length reversedConversions then
                    None
                else
                    let (toStr, fromStr) = reversedConversions[index]
                    let idx = currentMolecule.IndexOf(toStr)
                    if idx >= 0 then
                        Some (currentMolecule.Substring(0, idx) + fromStr + currentMolecule.Substring(idx + toStr.Length))
                    else
                        tryReplace (index + 1)

            match tryReplace 0 with
            | Some newMolecule -> greedyReduce newMolecule (steps + 1)
            | None -> failwithf "Stuck, cannot reduce molecule further: %s" currentMolecule

    greedyReduce molecule 0

let part2 (input: string[]) =
    parseInput input
    |> runPart2