/// https://adventofcode.com/2024/day/5
module Year2024Day5

open System

let parseInputs (input: string[]) =
    //Parse all of the rules and lines to integers
    let (parsedRules, parsedLines) =
        (([], []), input)
        ||> Array.fold (fun (rules, lines) next ->
            if String.IsNullOrWhiteSpace next then
                (rules, lines)
            elif next.Contains("|") then
                match next.Split("|") with
                | [| x; y |] -> ((int x, int y)::rules, lines)
                | _ -> (rules, lines)
            else
                next.Split(",")
                |> Seq.map (fun x -> int x)
                |> Seq.toList
                |> fun result -> (rules, result::lines)
        )

    //Creates a left row to right row entries map
    let invalidRulesMap =
        parsedRules
        |> List.groupBy snd
        |> List.map (fun (key, values) -> (key, values |> List.map fst))
        |> Map.ofList

    (invalidRulesMap, parsedLines)

let tryFindInvalidIndexes (invalidRulesMap: Map<int, int list>) (line: int list) =
    line
    |> List.indexed
    |> List.tryPick (fun (index, number) ->
        if (index = line.Length - 1) then
            //Minor optimization at the end of the list -> stop checking.
            None
        else
            match invalidRulesMap |> Map.tryFind number with
            | Some invalidRules ->
                match line |> List.skip index |> List.tryFindIndex (fun nextNumber -> invalidRules |> List.contains(nextNumber)) with
                | Some invalidIndex ->
                    //Found an invalid entry for index on index + invalidIndex.
                    Some (index, index + invalidIndex)
                | None ->
                    //Nothing invalid, carry on.
                    None
            | None ->
                //No invalid rules found -> skip
                None)

let part1 (input: string[]) =
    let (invalidRulesMap, parsedLines) = parseInputs input
    let conformsToTheRules = tryFindInvalidIndexes invalidRulesMap >> Option.isNone

    parsedLines
    |> List.sumBy (fun line ->
        if conformsToTheRules line then
            line[(line.Length - 1) / 2]
        else
            0
    )

let part2 (input: string[]) =
    let (invalidRulesMap, parsedLines) = parseInputs input
    let tryFindInvalidIndexes = tryFindInvalidIndexes invalidRulesMap

    let makeValid (line: int list) =
        //This could probably be faster using a C# list, but it's fast enough...
        let swap (x: int) (y: int) (line: int list) =
            line
            |> List.updateAt x line[y]
            |> List.updateAt y line[x]

        //Keep on reordering until the line is valid
        let rec reorder (line: int list) =
            match tryFindInvalidIndexes line with
            | Some (left, right) -> reorder (line |> swap left right)
            | None -> line

        reorder line


    parsedLines
    |> List.sumBy (fun line ->
        let reorderedLine = makeValid line
        if line = reorderedLine then 0 else reorderedLine[(reorderedLine.Length - 1) / 2]
    )