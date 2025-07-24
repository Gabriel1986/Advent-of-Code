/// https://adventofcode.com/2015/day/16
// There are no tests defined for this day, so I made some up... Should return 3 for part 1 and 4 for part2
module Year2015Day16


open System.Text.RegularExpressions

let sueRegex = Regex("^Sue \d+: (\w+): (\d+), (\w+): (\d+), (\w+): (\d+)$", RegexOptions.Compiled)

let parseInput (input: string[]) =
    input
    |> Seq.indexed
    |> Seq.map (fun (i, line) ->
        let m = sueRegex.Match(line)
        i + 1,
        seq {
            yield m.Groups[1].Value, int m.Groups[2].Value
            yield m.Groups[3].Value, int m.Groups[4].Value
            yield m.Groups[5].Value, int m.Groups[6].Value
        })

let tickerTape =
    [
        "children", 3
        "cats", 7
        "samoyeds", 2
        "pomeranians", 3
        "akitas", 0
        "vizslas", 0
        "goldfish", 5
        "trees", 3
        "cars", 2
        "perfumes", 1
    ]
    |> dict

let part1 (input: string[]) =
    parseInput input
    |> Seq.find (fun (_, sueProps) ->
        sueProps
        |> Seq.forall (fun (property, value) ->
            match tickerTape.TryGetValue(property) with
            | true, x -> x = value
            | false, _ ->  true))
    |> fst

let part2 (input: string[]) =
    parseInput input
    |> Seq.find (fun (_, sueProps) ->
        sueProps
        |> Seq.forall (fun (property, value) ->
            match tickerTape.TryGetValue(property) with
            | true, x ->
                match property with
                | "cats" | "trees" -> x < value
                | "pomeranians" | "goldfish" -> x > value
                | _ -> x = value
            | false, _ ->
                true))
    |> fst