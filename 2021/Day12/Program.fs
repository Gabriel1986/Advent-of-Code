/// https://adventofcode.com/2021/day/12
module Year2021Day12
open System

type Cave =
    | Start
    | End
    | LargeCavern of string
    | SmallCave of string

let private parseInput (input: string array) =
    let parseCavern (str: string) =
        match str with
        | "start" -> Start
        | "end" -> End
        | x when x |> String.forall Char.IsLower -> SmallCave x
        | x when x |> String.forall Char.IsUpper -> LargeCavern x
        | other -> failwithf "Unexpected string: %s" other

    let createLinks (leftCavernString: string, rightCavernString: string) =
        seq {
            match parseCavern leftCavernString, parseCavern rightCavernString with
            //End is an end link
            | End, otherCavern | otherCavern, End ->
                otherCavern, End
            //Start is a start link
            | Start, otherCavern | otherCavern, Start ->
                Start, otherCavern
            //Other types of cavern are double linked with each-other
            | leftCavern, rightCavern ->
                leftCavern, rightCavern
                rightCavern, leftCavern
        }

    let addLinkToMap linkMap (x, y) =
        linkMap |> Map.change x (function None -> Some (y::[]) | Some list -> Some (y::list))

    input
    |> Array.fold (fun linkMap linkString ->
        let linkStrings = linkString.Split("-")
        createLinks (linkStrings[0], linkStrings[1])
        |> Seq.fold addLinkToMap linkMap
    ) Map.empty

let part1 (input) =
    let mapOfTheCaves = parseInput (input)

    let rec travel (currentCave: Cave, visitedSmallCaves: Set<string>, stack: Cave list) =
        match currentCave with
        | End -> [ currentCave::stack ]
        | SmallCave x when visitedSmallCaves.Contains(x) ->
            []
        | SmallCave x ->
            mapOfTheCaves[currentCave]
            |> List.collect (fun linkedCave -> travel(linkedCave, visitedSmallCaves.Add x, currentCave::stack))
        | _other ->
            mapOfTheCaves[currentCave]
            |> List.collect (fun x -> travel(x, visitedSmallCaves, currentCave::stack))

    let possibleTraversals = travel (Start, Set.empty, [])
    possibleTraversals.Length

let part2 (input) =
    let mapOfTheCaves = parseInput (input)

    let rec travel (currentCave: Cave, visitedSmallCaves: Map<string, int>, stack: Cave list) =
        match currentCave with
        | End -> [ List.rev (currentCave::stack) ]
        | SmallCave smallCave ->
            //If we've already visited the small cave, check if we can visit it again
            if visitedSmallCaves.ContainsKey smallCave && (visitedSmallCaves |> Map.exists (fun _smallCave nbTimesVisited -> nbTimesVisited = 2)) then
                []
            else
                mapOfTheCaves[currentCave]
                |> List.collect (fun link -> travel (link, visitedSmallCaves.Change (smallCave, (function Some nbTimesVisited -> Some (nbTimesVisited + 1) | None -> Some 1)), currentCave::stack))
        | _other ->
            mapOfTheCaves[currentCave]
            |> List.collect (fun link -> travel (link, visitedSmallCaves, currentCave::stack))

    let possibleTraversals = travel (Start, Map.empty, [])
    possibleTraversals.Length