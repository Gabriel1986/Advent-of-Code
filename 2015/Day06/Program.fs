/// https://adventofcode.com/2015/day/6
// Doing this without mutability increases the time with 10x -> mutable collections it is then.
module Year2015Day6

open System.Text.RegularExpressions
open System.Collections

// Define types for clarity
type Command =
    | TurnOn
    | TurnOff
    | Toggle

type Position = { X: int; Y: int }

type Instruction = {
    Command: Command
    From: Position
    To: Position
}

// Parse a single line into an Instruction
let parseInstruction (line: string): Instruction =
    let pattern = @"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)"
    let m = Regex.Match(line, pattern)
    if not m.Success then failwithf "Invalid line: %s" line

    let command =
        match m.Groups.[1].Value with
        | "turn on" -> TurnOn
        | "turn off" -> TurnOff
        | "toggle" -> Toggle
        | other -> failwithf "Unknown command: %s" other

    let pos (a: int) (b: int) = { X = int m.Groups.[a].Value; Y = int m.Groups.[b].Value }

    {
        Command = command
        From = pos 2 3
        To = pos 4 5
    }

let applyInstructionPart1 (grid: BitArray) (instr: Instruction) =
    for y in instr.From.Y .. instr.To.Y do
        let rowStart = y * 1000
        for x in instr.From.X .. instr.To.X do
            let index = rowStart + x
            match instr.Command with
            | TurnOn -> grid.[index] <- true
            | TurnOff -> grid.[index] <- false
            | Toggle -> grid.[index] <- not grid.[index]

let applyInstructionPart2 (grid: int[]) (instr: Instruction) =
    for y in instr.From.Y .. instr.To.Y do
        let rowStart = y * 1000
        for x in instr.From.X .. instr.To.X do
            let i = rowStart + x
            match instr.Command with
            | TurnOn -> grid.[i] <- grid.[i] + 1
            | TurnOff -> grid.[i] <- max 0 (grid.[i] - 1)
            | Toggle -> grid.[i] <- grid.[i] + 2

let part1 (lines: string[]) =
    let grid = BitArray(1_000_000, false)
    lines
    |> Seq.map parseInstruction
    |> Seq.iter (applyInstructionPart1 grid)

    let mutable count = 0
    for i in 0 .. grid.Length - 1 do
        if grid.[i] then count <- count + 1
    count

let part2 (lines: string[]) =
    let grid = Array.zeroCreate<int> 1_000_000

    lines
    |> Seq.map parseInstruction
    |> Seq.iter (applyInstructionPart2 grid)

    Array.sum grid