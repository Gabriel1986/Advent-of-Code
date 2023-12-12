/// https://adventofcode.com/2021/day/9
module Year2021Day9
open System

let part1 (input: string array) =
    let heightMap: int[][] = [|
        for line in input do
            [|
                for character in line do
                    Int32.Parse (character.ToString())
            |]
    |]

    let lowPoints = seq {
        for i in 0..heightMap.Length-1 do
            for j in 0..heightMap[i].Length-1 do
                let currentNumber = heightMap[i][j]

                let allNeighborsAreLarger =
                    [
                        if i > 0 then heightMap[i-1][j] else Int32.MaxValue
                        if i < heightMap.Length-1 then heightMap[i+1][j] else Int32.MaxValue
                        if j > 0 then heightMap[i][j-1] else Int32.MaxValue
                        if j < heightMap[i].Length-1 then heightMap[i][j+1] else Int32.MaxValue
                    ]
                    |> List.forall (fun x -> currentNumber < x)

                if allNeighborsAreLarger then currentNumber else ()
    }

    lowPoints
    |> Seq.sumBy ((+) 1)

type Point =
    {
        Number: Int64
        Visited: bool
    }
    member me.Impassible = me.Number = 9

let part2 (input: string array) =
    let heightMap =
        Array2D.init (input.Length) (input[0].Length) (fun i j ->
            { Number = Int64.Parse (string (input[i][j])); Visited = false }
        )

    let countBasin (i: int, j: int): Int64 =
        let rec visitNeighbor (i: int, j: int) =
            if i < 0 || i >= heightMap.GetLength(0) || j < 0 || j >= heightMap.GetLength(1) then
                0
            else
                let currentPoint = heightMap[i, j]
                if currentPoint.Impassible || currentPoint.Visited then
                    0
                else
                    heightMap[i, j] <- { currentPoint with Visited = true }
                    1 + visitNeighbor (i+1, j) + visitNeighbor (i, j+1) + visitNeighbor (i-1, j) + visitNeighbor (i, j-1)
        visitNeighbor (i, j)

    let basinSizes = seq {
        for i in 0..heightMap.GetUpperBound(0) do
            for j in 0..heightMap.GetUpperBound(1) do
                let currentNumber = heightMap[i, j]
                if currentNumber.Impassible || currentNumber.Visited then
                    ()
                else
                    countBasin (i, j)
    }

    basinSizes
    |> Seq.sortDescending
    |> Seq.take 3
    |> Seq.fold (fun acc next -> acc * bigint next) (1I)