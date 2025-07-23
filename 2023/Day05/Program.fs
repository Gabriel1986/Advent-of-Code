/// https://adventofcode.com/2023/day/5
module Year2023Day5
open System

type MinMax = { Min: bigint; Max: bigint }

let numberRegex = Regex.numberRegex

let convertToMinMax (line: string): (MinMax * MinMax) =
    let minMax = line.Split(" ")
    let range = bigint.Parse minMax[2]
    let destination = bigint.Parse minMax[0]
    let source = bigint.Parse minMax[1]
    { Min = source; Max = source + range - bigint 1 }, { Min = destination; Max = destination + range - bigint 1 }


let parseInput (input: string array) =
    (([], []), input)
    ||> Array.fold (fun (seeds, maps: (MinMax * MinMax) list list) line ->
        if String.IsNullOrWhiteSpace(line) then
            (seeds, maps)
        elif line.StartsWith("seeds:") then
            let theSeeds =
                line
                |> numberRegex.Matches
                |> Seq.map (fun each -> bigint.Parse each.Value)
                |> Seq.toList
            (theSeeds, maps)
        elif line.Contains ":" then
            (seeds, []::maps)
        else
            match maps with
            | x::xs -> (seeds, ((convertToMinMax line)::x)::xs)
            | [] -> (seeds, [[ convertToMinMax line ]])
    )
    |> fun (seeds, maps) -> (seeds, maps |> List.rev)

let tryTranslateValue (value: bigint) (source: MinMax, destination: MinMax) =
    if value >= source.Min && value <= source.Max then
        Some (value - source.Min + destination.Min)
    else
        None

let mapSeedToLocation (translationMaps: (MinMax * MinMax) list list) (seed: bigint) =
    (seed, translationMaps)
    ||> Seq.fold (fun currentValue maps ->
        maps
        |> Seq.tryPick (tryTranslateValue currentValue)
        |> Option.defaultValue (currentValue))

let part1 (input) =
    let (seeds, maps) = parseInput input

    seeds
    |> Seq.map (mapSeedToLocation maps)
    |> Seq.min

type SeedRangePart =
    | Original of MinMax
    | Translated of MinMax * MinMax
    member me.OriginalValue =
        match me with
        | Original minMax -> minMax
        | Translated (minMax, _) -> minMax
    member me.TranslatedValue =
        match me with
        | Original minMax -> minMax
        | Translated (_, minMax) -> minMax

let splitIntoSmallerParts (translationMap: (MinMax * MinMax) list) (seed: MinMax): MinMax list =
    ([Original seed], translationMap)
    ||> List.fold (fun splitSeeds (source, destination) ->
        splitSeeds
        |> List.collect (fun splitSeed ->
            let splitSeedValue = splitSeed.OriginalValue
            //Detect if the seed is in the source range
            if splitSeedValue.Min <= source.Max && splitSeedValue.Max >= source.Min then
                [
                    //If the seed is smaller than the source -> make a new min max starting from splitSeed min to source min - 1
                    if (splitSeedValue.Min < source.Min) then
                        Original { Min = splitSeedValue.Min; Max = source.Min - bigint 1 }

                    //If the seed is bigger than the source -> make a new min max starting from source max + 1 to splitSeed max
                    if (splitSeedValue.Max > source.Max) then
                        Original { Min = source.Max + bigint 1; Max = splitSeedValue.Max }

                    //Mark the rest of the seed for translation
                    Translated ({
                        Min = if splitSeedValue.Min >= source.Min then splitSeedValue.Min else source.Min
                        Max = if splitSeedValue.Max <= source.Max then splitSeedValue.Max else source.Max
                    }, {
                        Min = if splitSeedValue.Min > source.Min then destination.Min + (splitSeedValue.Min - source.Min) else destination.Min
                        Max = if splitSeedValue.Max < source.Max then destination.Max - (source.Max - splitSeedValue.Max) else destination.Max
                    })
                ]
            //Else just return the seed
            else [ splitSeed ]))
    //Perform the actual translation after the translation maps have been applied
    |> List.map (fun each -> each.TranslatedValue)

let translateSeedRange (translationMaps: (MinMax * MinMax) list list) (seed: MinMax): List<MinMax> =
    ([seed], translationMaps)
    ||> List.fold (fun acc next ->
        acc |> List.collect (splitIntoSmallerParts next)
    )

let part2 (input) =
    let (seeds, maps) = parseInput input
    seeds
    |> Seq.chunkBySize  2
    |> Seq.map (fun arr -> { Min = arr[0]; Max = arr[0] + arr[1] - bigint 1 })
    |> Seq.map (translateSeedRange maps >> List.minBy _.Min >> _.Min)
    |> Seq.min