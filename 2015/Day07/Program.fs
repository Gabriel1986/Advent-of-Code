/// https://adventofcode.com/2015/day/7
module Year2015Day7

open System
open System.Collections.Generic

type Wire = string

type Operand =
    | Signal of uint16
    | Wire of Wire

type Command =
    | Value of Operand
    | And of Operand * Operand
    | Or of Operand * Operand
    | Not of Operand
    | LShift of Wire * int
    | RShift of Wire * int

type Instruction = {
    Command: Command
    Destination: string
}

let parseInstruction (line: string) : Instruction =
    let parts = line.Split(" -> ")
    let destination = parts.[1].Trim()

    let operand (x: string): Operand =
        match UInt16.TryParse x with
        | true, value -> Signal value
        | false, _ -> Wire x

    let command =
        match parts.[0].Trim().Split() with
        | [| "NOT"; x |] ->
            Not (operand x)

        | [| a; "AND"; b |] ->
            And (operand a, operand b)

        | [| a; "OR"; b |] ->
            Or (operand a, operand b)

        | [| a; "LSHIFT"; n |] ->
            LShift (a, int n)

        | [| a; "RSHIFT"; n |] ->
            RShift (a, int n)

        | [| x |] ->
            Value (operand x)

        | _ ->
            failwithf "Unrecognized instruction: %s" line

    {
        Command = command
        Destination = destination
    }

let applyInstruction (dictionary: Dictionary<string, uint16>) (instruction: Instruction) =
    let getValue (operand: Operand) =
        match operand with
        | Signal x -> x
        | Wire x -> dictionary[x]

    let result =
        match instruction.Command with
        | Value operand -> getValue operand
        | And (operand1, operand2) -> getValue operand1 &&& getValue operand2
        | Or (operand1, operand2) -> getValue operand1 ||| getValue operand2
        | Not operand -> ~~~(getValue operand)
        | LShift (wire, pos) -> dictionary[wire] <<< pos
        | RShift (wire, pos) -> dictionary[wire] >>> pos

    dictionary[instruction.Destination] <- result

let findResult (wire: Wire) (cachedValues: Dictionary<Wire, uint16>) (instructions: Map<string, Command>) =
    let inline getCachedValue (wire: Wire) =
        match cachedValues.TryGetValue wire with
        | true, value -> Some value
        | false, _ -> None

    let rec findValue (wire: Wire) =
        let inline findSignalValue (operand: Operand): uint16 =
            match operand with
            | Signal x ->
                x
            | Wire x ->
                findValue x

        match getCachedValue wire with
        | Some value -> value
        | None ->
            let result =
                match instructions[wire] with
                | Value x ->
                    findSignalValue x
                | And (x, y) ->
                    findSignalValue x &&& findSignalValue y
                | Or (x, y) ->
                    findSignalValue x ||| findSignalValue y
                | Not x ->
                    ~~~(findSignalValue x)
                | LShift (x, y) ->
                    findValue x <<< y
                | RShift (x, y) ->
                    findValue x >>> y

            cachedValues.Add(wire, result)
            result

    findValue wire

let part1 (input: string[]) =
    input
    |> Array.map parseInstruction
    |> Array.map (fun instruction -> instruction.Destination, instruction.Command)
    |> Map.ofArray
    |> findResult "a" (new Dictionary<string, uint16>())
    |> int

let part2 (input: string[]) =
    let mapOfInstructions =
        input
        |> Array.map parseInstruction
        |> Array.map (fun instruction -> instruction.Destination, instruction.Command)
        |> Map.ofArray

    //Find the result for wire A
    let resultOfA =
        mapOfInstructions
        |> findResult "a" (new Dictionary<string, uint16>())

    //Override wire B to the value of wire A
    let newCachedDictionary = new Dictionary<string, uint16>()
    newCachedDictionary.Add("b", resultOfA)

    //Then find the value of wire A again with the new setting for wire B
    mapOfInstructions
    |> findResult "a" newCachedDictionary
    |> int
