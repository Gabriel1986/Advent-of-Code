/// https://adventofcode.com/2021/day/5
module Year2021Day5
open System

type Point = { X: int; Y: int }

let private parseInput (input: string array) =
    let toTuple (str: string): (Point * Point) =
        let parseNumbers (subString: string) =
            subString.Split(",")
            |> (fun x -> { X = Int32.Parse x[0]; Y = Int32.Parse x[1] })

        str.Split(" -> ")
        |> (fun x -> parseNumbers x[0], parseNumbers x[1])

    input
    |> Array.map toTuple

let part1 (input) =
    let lineSegments =
        input
        |> parseInput
        |> Seq.filter (fun (p1, p2) -> p1.X = p2.X || p1.Y = p2.Y)

    let generatePoints (point1: Point, point2: Point) = [
        if (point1.X = point2.X) then
            let y1, y2 = Math.Min(point1.Y, point2.Y), Math.Max(point1.Y, point2.Y)
            for i = y1 to y2 do yield { X = point1.X; Y = i }
        else
            let x1, x2 = Math.Min(point1.X, point2.X), Math.Max(point1.X, point2.X)
            for i = x1 to x2 do yield { X = i; Y = point1.Y }
    ]

    let mapOfOverlaps =
        lineSegments
        |> Seq.fold (fun (alreadySeen: Map<Point, int>) nextSegment ->
            generatePoints nextSegment
            |> List.fold (fun alreadySeen point ->
                alreadySeen.Change (point, function None -> Some 1 | Some overlap -> Some (overlap+1))) alreadySeen) Map.empty

    mapOfOverlaps.Values
    |> Seq.sumBy (fun each -> if each > 1 then 1 else 0)

let part2 (input) =
    let lineSegments =
        input
        |> parseInput
        |> Seq.filter (fun (p1, p2) -> p1.X = p2.X || p1.Y = p2.Y || Math.Abs(p1.X - p2.X) = Math.Abs (p1.Y - p2.Y))

    //Can probably be cleaned up, but CBA
    let generatePoints (point1: Point, point2: Point) = seq {
        if (point1.X = point2.X) then
            let y1, y2 = Math.Min(point1.Y, point2.Y), Math.Max(point1.Y, point2.Y)
            for i = y1 to y2 do yield { X = point1.X; Y = i }
        elif (point1.Y = point2.Y) then
            let x1, x2 = Math.Min(point1.X, point2.X), Math.Max(point1.X, point2.X)
            for i = x1 to x2 do yield { X = i; Y = point1.Y }
        else
            //(1, 1) -> (3, 3)
            if (point1.X < point2.X && point1.Y < point2.Y) then
                for i = 0 to point2.X - point1.X do
                    yield { X = point1.X + i; Y = point1.Y + i }
            //(10, 6) -> (7, 9)
            if (point1.X > point2.X && point1.Y < point2.Y) then
                for i = 0 to point1.X - point2.X do
                    yield { X = point2.X + i; Y = point2.Y - i }
            //(7, 9) -> (10, 6)
            if (point1.X < point2.X && point1.Y > point2.Y) then
                for i = 0 to point2.X - point1.X do
                    yield { X = point1.X + i; Y = point1.Y - i }
            //(3, 3) -> (1, 1)
            if (point1.X > point2.X && point1.Y > point2.Y) then
                for i = 0 to point1.X - point2.X do
                    yield { X = point2.X + i; Y = point2.Y + i }
    }

    let mapOfOverlaps =
        lineSegments
        |> Seq.fold (fun (alreadySeen: Map<Point, int>) nextSegment ->
            generatePoints nextSegment
            |> Seq.fold (fun alreadySeen point ->
                alreadySeen.Change (point, function None -> Some 1 | Some overlap -> Some (overlap+1))) alreadySeen) Map.empty

    mapOfOverlaps.Values
    |> Seq.sumBy (fun each -> if each > 1 then 1 else 0)