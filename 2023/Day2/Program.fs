/// https://adventofcode.com/2023/day/2
module Year2023Day2
open System
open System.Text.RegularExpressions

let digitRegex = Regex(@"\d+")

type DiePull = { DieColor: string; NumberOfDice: int }

//Optimised => For both problems we don't need to have the 3 individual bag pulls (that would be a sequence of die pull and lead to a sequence of a sequence of die pulls...).
//Having a single sequence containing ALL the die pulls of a single game is good enough :)
let parseDiePulls (line: string) =
    //Separate pulls from the bag
    line.Split(":").[1].Split(";")
    //Separate dice from each pull
    |> Seq.collect (fun x -> x.Split(","))
    //Separate die color from number of dice
    |> Seq.map (fun x -> x.Trim().Split(" "))
    |> Seq.map (fun x -> 
        let dieColor = x[1]
        let nbDice = Int32.Parse x[0]
        { DieColor = dieColor; NumberOfDice = nbDice })

let part1 (input) =
    let isImpossible (diePull: DiePull) =
        match diePull.DieColor with
        | "red" -> 12
        | "green" -> 13
        | "blue" -> 14
        | _ -> failwith "Unknown die color"
        |> fun max -> diePull.NumberOfDice > max

    input
    |> Array.sumBy (fun line ->
        let gameNumber = Int32.Parse(digitRegex.Match(line).Value)

        let gameIsImpossible =
            line
            |> parseDiePulls
            |> Seq.exists isImpossible

        if gameIsImpossible then 0 else gameNumber
    )

let part2 (input) =
    input
    |> Array.sumBy (fun line ->
        line
        |> parseDiePulls
        |> Seq.groupBy _.DieColor
        |> Seq.map (snd >> Seq.maxBy _.NumberOfDice >> _.NumberOfDice)
        |> Seq.reduce (*))