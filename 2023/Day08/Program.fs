/// https://adventofcode.com/2023/day/8
module Year2023Day8
open System.Numerics
open System.Text.RegularExpressions

let private nodeRegex = Regex(@"[A-Z0-9]{3}", RegexOptions.Compiled)

let private parseInput (input: string array) =
    let instructions =
        input[0]
        |> Seq.map (function 'L' -> fst | _ -> snd)
        |> Seq.toList
    let nodes =
        input[2..]
        |> Array.map (nodeRegex.Matches >> Seq.toList >> function x -> (x[0].Value, (x[1].Value, x[2].Value)))
        |> Map.ofArray
    (instructions, nodes)


let private traverse (map: Map<string, (string * string)>) (instructions: (string * string -> string) list) (stopCondition: string -> bool) (startingNode: string) =
    let rec traverse (node: string) (totalNbSteps: bigint) =
        ((node, totalNbSteps, false), instructions)
        ||> Seq.fold (fun (currentNode, nbSteps, isFinished) nextInstruction ->
            if isFinished || stopCondition(currentNode) then
                currentNode, nbSteps, true
            else
                nextInstruction (map |> Map.find currentNode), nbSteps + bigint 1, false
        )
        |> fun (stopNode, stopNbSteps, isFinished) ->
            if isFinished then stopNbSteps else traverse stopNode stopNbSteps
    traverse startingNode BigInteger.Zero

let part1 (input) =
    let (instructions, map) = parseInput input
    traverse map instructions (fun node -> node = "ZZZ") "AAA"


let part2 (input) =
    let (instructions, map) = parseInput input

    let lcm  (a: bigint) (b: bigint) =
        let rec gcd (a: bigint) (b: bigint) = if b = BigInteger.Zero then a else gcd b (a % b);
        a * b / (gcd a b);

    map.Keys
    |> Seq.filter (fun each -> each.EndsWith "A")
    |> Seq.map (traverse map instructions (fun node -> node.EndsWith "Z"))
    |> Seq.fold lcm (bigint 1)