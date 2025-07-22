/// https://adventofcode.com/2015/day/10
module Year2015Day10

open System
open System.Linq

//Once again I had to go with a mutable class to avoid creating a new sequence all the time.
let evolve (input: ResizeArray<int>): ResizeArray<int> =
    let result = ResizeArray<int>()

    //Had to use recursion as folding over the input is about 3x slower.
    let rec loop index current count =
        if index >= input.Count then
            result.Add(count)
            result.Add(current)
        else
            let next = input[index]
            if next = current then
                loop (index + 1) current (count + 1)
            else
                result.Add(count)
                result.Add(current)
                loop (index + 1) next 1

    loop 1 input[0] 1
    result

let parse (str: string) =
    str.ToCharArray()
    |> Array.map (string >> Int32.Parse)
    |> fun x -> new ResizeArray<int>(x)

let runPart1 (input: string[]) (iterations: int) =
    let parsed = parse input[0]
    (parsed, seq { 1..iterations })
    ||> Seq.fold (fun acc _ -> evolve acc)

let testPart1 (input: string[]) =
    runPart1 input 1

let part1 (input: string[]) =
    runPart1 input 40
    |> Seq.length

let part2 (input: string[]) =
    runPart1 input 50
    |> Seq.length