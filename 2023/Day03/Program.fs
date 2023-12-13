/// https://adventofcode.com/2023/day/3
module Year2023Day3
open System.Text.RegularExpressions

let part1 (input) =
    let numberRegex = Regex("[0-9]+")
    let specialCharacterRegex = Regex("[^0-9\\.]")

    //row * (row * column)
    let indexesOfSpecialCharacters =
        input
        |> Array.indexed
        |> Seq.collect (fun (rowIndex, line) ->
            specialCharacterRegex.Matches(line)
            |> Seq.map (fun aMatch -> (rowIndex, aMatch.Index))
        )
        |> Seq.groupBy fst
        |> Map.ofSeq

    let isNearSpecialCharacter (rowIndex: int) (columnIndex: int) (length: int) =
        seq {
            yield! indexesOfSpecialCharacters |> Map.tryFind (rowIndex - 1) |> Option.defaultValue []
            yield! indexesOfSpecialCharacters |> Map.tryFind rowIndex |> Option.defaultValue []
            yield! indexesOfSpecialCharacters |> Map.tryFind (rowIndex + 1) |> Option.defaultValue []
        }
        |> Seq.exists (fun (specialCharacterRowIndex, specialCharacterColumnIndex) ->
            specialCharacterColumnIndex >= columnIndex - 1 && specialCharacterColumnIndex <= columnIndex + length)

    input
    |> Array.indexed
    |> Array.sumBy (fun (rowIndex, line) ->
        numberRegex.Matches(line)
        |> Seq.sumBy (fun aMatch -> if isNearSpecialCharacter rowIndex aMatch.Index aMatch.Length then int aMatch.Value else 0)
    )

type RowIndex = int
type ColumnIndex = int
type Star = { RowIndex: RowIndex; ColumnIndex: ColumnIndex }
type ValueNearStar = { Star: Star; Value: int }

let part2 (input) =
    let numberRegex = Regex("[0-9]+")
    let specialCharacterRegex = Regex("[\\*]")

    //rowIndex * Star
    let indexesOfStars =
        input
        |> Array.indexed
        |> Seq.collect (fun (rowIndex, line) ->
            specialCharacterRegex.Matches(line)
            |> Seq.map (fun aMatch -> { RowIndex = rowIndex; ColumnIndex = aMatch.Index })
        )
        |> Seq.groupBy _.RowIndex
        |> Map.ofSeq

    let getNearestStars (rowIndex: RowIndex) (columnIndex: ColumnIndex) (length: int) =
        seq {
            yield! indexesOfStars |> Map.tryFind (rowIndex - 1) |> Option.defaultValue []
            yield! indexesOfStars |> Map.tryFind rowIndex |> Option.defaultValue []
            yield! indexesOfStars |> Map.tryFind (rowIndex + 1) |> Option.defaultValue []
        }
        |> Seq.filter (fun star -> star.ColumnIndex >= columnIndex - 1 && star.ColumnIndex <= columnIndex + length)

    input
    |> Array.indexed
    |> Seq.collect (fun (rowIndex, line) ->
        numberRegex.Matches(line)
        |> Seq.collect (fun aMatch ->
            getNearestStars rowIndex aMatch.Index aMatch.Length
            |> Seq.map (fun star -> { Star = star; Value = int aMatch.Value })))
    |> Seq.groupBy _.Star
    |> Seq.sumBy (fun (_, valuesNearStar) ->
        if valuesNearStar |> Seq.length = 2 then valuesNearStar |> Seq.multiplyBy (fun x -> x.Value) else 0)