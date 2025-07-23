/// https://adventofcode.com/2015/day/13
module Year2015Day13

open System
open System.Text.RegularExpressions

let happinessRegex = Regex(@"^([A-Za-z]+) would (gain|lose) (\d+) happiness units by sitting next to ([A-Za-z]+)\.", RegexOptions.Compiled)
let parseHappiness (input: string) =
    let matches = happinessRegex.Match(input)
    let person1 = matches.Groups[1].Value
    let sign = if matches.Groups[2].Value = "gain" then 1 else -1
    let amountOfHappiness = Int32.Parse(matches.Groups[3].Value) * sign
    let person2 = matches.Groups[4].Value
    (person1, person2), amountOfHappiness

let personRegex = Regex(@"^[A-Za-z]+", RegexOptions.Compiled)
let parsePerson (input: string) =
    let matches = personRegex.Match(input)
    matches.Value

let permuteNonCircular (people: string list) =
    let first = List.head people
    let rest = List.tail people

    rest
    |> List.permute
    |> Seq.map (fun perm -> first :: perm)

let runAlgorithm (persons: string list) (determineHappiness: string -> string -> int) =
    (Int32.MinValue, permuteNonCircular persons)
    ||> Seq.fold (fun maxValue nextPermutation ->
        let result =
            let rec loop acc prev =
                function
                | [] -> acc + determineHappiness prev nextPermutation.Head
                | x::xs -> loop (acc + determineHappiness prev x) x xs
            loop 0 nextPermutation.Head nextPermutation.Tail

        if result > maxValue then result else maxValue)

let parseHappinessDictionary (input: string[]) =
    let happinessDictionary =
        input
        |> Array.map parseHappiness
        |> dict

    //Create a dictionary that makes lookup simpler -> lookup[(x, y)] = lookup[(y, x)]
    //This makes it so we can then avoid double lookups in determineHappiness
    happinessDictionary.Keys
    |> Seq.map (fun (p1, p2) -> (p1, p2), happinessDictionary[(p1, p2)] + happinessDictionary[(p2, p1)])
    |> dict

let parsePersons (input: string[]) =
    input
    |> Array.map parsePerson
    |> Set.ofArray
    |> Set.toList

let part1 (input: string[]) =
    let happinessDictionary = parseHappinessDictionary input
    let persons = parsePersons input

    let determineHappiness person1 person2 =
        happinessDictionary[(person1, person2)]

    runAlgorithm persons determineHappiness

let part2 (input: string[]) =
    let happinessDictionary = parseHappinessDictionary input
    let persons = parsePersons input

    let determineHappiness person1 person2 =
        if person1 = "me" || person2 = "me" then
            0
        else
            happinessDictionary[(person1, person2)]

    runAlgorithm ("me" :: persons) determineHappiness