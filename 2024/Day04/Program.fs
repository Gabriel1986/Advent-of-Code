/// https://adventofcode.com/2024/day/4
module Year2024Day4

open System.Text.RegularExpressions

let part1 (input: string[]) =
    let sumXMas (indexedArray: char array2d) (x: int, y: int) =
        [
            //up
            [ (x, y-1); (x, y-2); (x, y-3) ]
            //leftUpDiagonal
            [ (x-1, y-1); (x-2, y-2); (x-3, y-3) ]
            //rightUpDiagonal
            [ (x+1, y-1); (x+2, y-2); (x+3, y-3) ]

            //down
            [ (x, y+1); (x, y+2); (x, y+3) ]
            //leftDownDiagonal
            [ (x-1, y+1); (x-2, y+2); (x-3, y+3) ]
            //rightDownDiagonal
            [ (x+1, y+1); (x+2, y+2); (x+3, y+3) ]

            //right
            [ (x+1, y); (x+2, y); (x+3, y) ]
            //left
            [ (x-1, y); (x-2, y); (x-3, y) ]
        ]
        |> List.sumBy (fun xyCoordinates ->
            xyCoordinates
            |> List.choose (fun (x, y) ->
                if x |> Math.BetweenInclusive 0 (indexedArray.GetLength(0) - 1) && y |> Math.BetweenInclusive 0 (indexedArray.GetLength(1) - 1) then
                    Some indexedArray[x, y]
                else
                    None
            )
            |> function [ 'M'; 'A'; 'S' ] -> 1 | _ -> 0
        )

    let indexedArray =
        input 
        |> Array.map (Seq.map id >> Seq.toArray)
        |> fun char2D -> Array2D.init char2D.Length char2D[0].Length (fun x y -> char2D[x][y])

    let sumXMas = sumXMas indexedArray

    let mutable sumOfXMasses = 0

    indexedArray
    |> Array2D.iteri (fun x y character ->
        if character = 'X' then
            sumOfXMasses <- sumOfXMasses + sumXMas (x, y))

    sumOfXMasses

let part2 (input: string[]) =
    let sumDoubleMas (indexedArray: char array2d) (x: int, y: int) =
        if x - 1 >= 0 && x + 1 < indexedArray.GetLength(0) && y - 1 >= 0 && y + 1 < indexedArray.GetLength(1) then
            [
                //backslash
                [ indexedArray[x-1, y-1]; indexedArray[x+1, y+1] ]
                //forwardslash
                [ indexedArray[x-1, y+1]; indexedArray[x+1, y-1] ]
            ]
            |> List.forall (
                function
                | [ 'M'; 'S' ]
                | [ 'S'; 'M' ] -> true
                | _ -> false)
            |> function true -> 1 | false -> 0
        else
            0

    let indexedArray =
        input 
        |> Array.map (Seq.map id >> Seq.toArray)
        |> fun char2D -> Array2D.init char2D.Length char2D[0].Length (fun x y -> char2D[x][y])

    let sumDoubleMas = sumDoubleMas indexedArray

    let mutable sumOfXMasses = 0

    indexedArray
    |> Array2D.iteri (fun x y character ->
        if character = 'A' then
            sumOfXMasses <- sumOfXMasses + sumDoubleMas (x, y))

    sumOfXMasses