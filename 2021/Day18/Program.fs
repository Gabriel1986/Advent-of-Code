/// https://adventofcode.com/2021/day/18
module Year2021Day18
open System
open System.Text.RegularExpressions

type Node = {
    Left: NodeType
    Right: NodeType
}
and NodeType =
    | Node of Node
    | Value of int

let depthsAndIndexesOfBrackets (str: string) =
    str
    |> Seq.indexed
    |> Seq.fold (fun (depthsAndIndexes, currentDepth) (index, next) ->
        match next with
        | '[' -> (currentDepth, index)::depthsAndIndexes, currentDepth + 1
        | ']' -> (currentDepth-1, index)::depthsAndIndexes, currentDepth - 1
        | other -> depthsAndIndexes, currentDepth
    ) ([], 0)
    |> (fst >> List.rev)

let parseNode (str: string): Node =
    let rec parseNode (str: string): NodeType =

        let relevantIndexes = str |> depthsAndIndexesOfBrackets |> List.choose (fun (depth, index) -> if depth = 1 then Some index else None)

        let parseInt (character: char) = Int32.Parse (string character)
        let leftNode, rightNode =
            match relevantIndexes.Length with
            | 4 ->
                let leftNode = parseNode (str[relevantIndexes[0]..relevantIndexes[1]])
                let rightNode = parseNode (str[relevantIndexes[2]..relevantIndexes[3]])
                leftNode, rightNode
            | 2 ->
                if relevantIndexes[0] = 1 then
                    let leftNode = parseNode(str[relevantIndexes[0]..relevantIndexes[1]])
                    let rightNode = Value (parseInt str[str.Length - 2])
                    leftNode, rightNode
                else
                    let leftNode = Value (parseInt str[1])
                    let rightNode = parseNode (str[relevantIndexes[0]..relevantIndexes[1]])
                    leftNode, rightNode
            | 0 ->
                let leftNode = Value (parseInt str[1])
                let rightNode = Value (parseInt str[str.Length-2])
                leftNode, rightNode
            | _ ->
                failwithf "Failed to match []"

        Node { Left = leftNode; Right = rightNode }

    match parseNode (str) with
    | Value v -> failwithf "Unexpected input: %i" v
    | Node node -> node

let private hasExplosion (str: string) =
    match depthsAndIndexesOfBrackets str |> List.filter (fun (depth, _) -> depth > 3) with
    | [] -> None
    | x::y::_ -> Some [ snd x; snd y ]
    | other -> failwithf "Failed to parse explosion: %A" other

let private hasSplit (str: string) =
    let m = Regex.Match(str, "\d\d")
    if m.Success then Some m.Value else None

let private findLastNumberRegex = new Regex("\d+", RegexOptions.RightToLeft ||| RegexOptions.Compiled)
let private findFirstNumberRegex = new Regex("\d+", RegexOptions.Compiled)

let private tryExplode (str: string) =
    match hasExplosion str with
    | Some explosion ->
        let toSplit = str[explosion[0]+1..explosion[1]-1]
        let splitExplosion = toSplit.Split(",")
        let leftValue = Int32.Parse splitExplosion[0]
        let rightValue = Int32.Parse splitExplosion[1]

        let firstPart =
            let partial = str[0..explosion[0]-1]
            let m = findLastNumberRegex.Match(partial)
            if m.Success then
                let toReplace = Int32.Parse(m.Value)
                findLastNumberRegex.Replace(partial, string (toReplace + leftValue), 1)
            else
                partial

        let secondPart =
            let partial = str[explosion[1]+1..]
            let m = findFirstNumberRegex.Match(partial)
            if m.Success then
                let toReplace = Int32.Parse(m.Value)
                findFirstNumberRegex.Replace(partial, string (toReplace + rightValue), 1, m.Index)
            else
                partial

        let exploded = firstPart + "0" + secondPart
        Some exploded
    | None ->
        None

let private trySplit (str: string) =
    match hasSplit str with
    | Some splittable ->
        let toSplit = Double.Parse(splittable)
        let regex = new Regex(splittable)
        let split = regex.Replace(str, $"[%i{Math.Floor(toSplit/2.) |> int},%i{Math.Ceiling(toSplit/2.) |> int}]", 1)
        Some split
    | None ->
        None

let rec tryExplodeOrSplit (str: string) =
    match str |> tryExplode |> Option.orElse (trySplit str) with
    | Some explodedOrSplit -> tryExplodeOrSplit explodedOrSplit
    | None -> str

let private addNode (str: string) (next: string) =
    tryExplodeOrSplit $"[{str},{next}]"

let calculateMagnitude (str: string) =
    let rec calculateMagnitude (node: Node) =
        let left =
            match node.Left with
            | Node node -> calculateMagnitude node
            | Value value -> value

        let right =
            match node.Right with
            | Node node -> calculateMagnitude node
            | Value value -> value

        3 * left +  2 * right
    calculateMagnitude (parseNode str)

let part1 (input: string array) =
    let result =
        input
        |> Array.skip 1
        |> Array.fold addNode input[0]

    calculateMagnitude result

let part2 (input: string array) =
    input
    |> Array.map (fun node ->
        input
        |> Array.filter ((<>) node)
        |> Array.map (addNode node >> calculateMagnitude)
        |> Array.max
    )
    |> Array.max