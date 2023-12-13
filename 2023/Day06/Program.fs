/// https://adventofcode.com/2023/day/6
module Year2023Day6

open System.Numerics
open System.Text.RegularExpressions

let digitRegex = Regex(@"\d+")

let parseInput (input: string array) =
    let times  = digitRegex.Matches input[0] |> Seq.map (fun m -> BigInteger.Parse m.Value)
    let distances = digitRegex.Matches input[1] |> Seq.map (fun m -> BigInteger.Parse m.Value)
    Seq.zip times distances

type LimitType = LeftLimit | RightLimit


let findNbSolutions (totalTime: bigint) (totalDistance: bigint) =
    let inline isBetterThanCurrentDistance (time: bigint) =
        time * (totalTime - time) > totalDistance

    let rec findLimit (limitType: LimitType) (smaller: bigint) (bigger: bigint) =
        if bigger - smaller <= bigint 3 then
            //Brute force it :)
            match limitType with
            | LeftLimit -> { smaller..bigger  }
            | RightLimit -> { smaller..bigger  } |> Seq.rev
            |> Seq.find isBetterThanCurrentDistance
        else
            //Binary search
            let halfway = smaller + ((bigger - smaller) / bigint 2)
            if halfway |> isBetterThanCurrentDistance then
                match limitType with
                | LeftLimit -> findLimit limitType (smaller) (halfway)
                | RightLimit -> findLimit limitType (halfway) (bigger)
            else
                match limitType with
                | LeftLimit -> findLimit limitType (halfway) (bigger)
                | RightLimit -> findLimit limitType (smaller) (halfway)

    let firstLimit = findLimit LeftLimit (bigint 0) (totalTime)
    let lastLimit = findLimit RightLimit (bigint 0) (totalTime)
    lastLimit - firstLimit + bigint 1

let part1 (input) =
    (bigint 1, parseInput input)
    ||> Seq.fold (fun acc (time, distance) -> acc * findNbSolutions time distance)

let part2 (input) =
    let totalTime, totalDistance =
        (("", ""), parseInput input)
        ||> Seq.fold (fun (totalTime, totalDistance) (time, distance) -> (totalTime + string time, totalDistance + string distance))
        |> fun (totalTime, totalDistance) -> (BigInteger.Parse totalTime,  BigInteger.Parse totalDistance)

    findNbSolutions totalTime totalDistance