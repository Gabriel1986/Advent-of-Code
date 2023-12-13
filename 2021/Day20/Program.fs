/// https://adventofcode.com/2021/day/20
module Year2021Day20
open System

let private parseInput (input: string array): string * char[][] =
    let enhancer = input[0]
    let image = input[2..]
    enhancer,
        [|
            for i in 0..image.Length-1 do [|
                for j in 0..image[0].Length-1 do
                    image[i][j]
            |]
        |]

let private enhance (times: int) (enhancer: string, image: char[][]) =
    let enhance (image: char[][]) (iteration: int) =
        let originalMaxI = image.Length - 1
        let originalMaxJ = image[0].Length - 1

        [|
            for i in -1..originalMaxI+1 do [|
                for j in -1..originalMaxJ+1 do
                    let square = String [|
                        for squareI in -1..1 do
                            for squareJ in -1..1 do
                                let originalI, originalJ = i + squareI, j + squareJ
                                if originalI < 0 || originalI > originalMaxI || originalJ < 0 || originalJ > originalMaxJ then
                                    if enhancer[0] = '#' then
                                        if iteration % 2 = 1 then '0' else '1'
                                    else
                                        '0'
                                elif image[originalI][originalJ] = '.' then
                                    '0'
                                else
                                    '1'
                    |]
                    enhancer[Convert.ToInt32(square, 2)]
            |]
        |]

    seq { 1..times }
    |> Seq.fold enhance image

let readLitPixels (image: char[][]) =
    image
    |> Array.sumBy (Array.sumBy (function '#' -> 1 | _ -> 0))

let part1 (input) =
    parseInput (input)
    |> enhance 2
    |> readLitPixels

let part2 (input) =
    parseInput (input)
    |> enhance 50
    |> readLitPixels