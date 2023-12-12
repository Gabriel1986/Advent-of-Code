/// https://adventofcode.com/2023/day/11
module Year2023Day11

let private parseInput (input: string array) (emptyMultiplier: bigint) =
    let indexesOfEmptyRows =
        input
        |> Array.indexed
        |> Array.choose (fun (idx, line) -> if line.Contains("#") then None else Some idx)

    let indexesOfEmptyColumns =
        let potentialIndexes =
            input[0]
            |> Seq.indexed
            |> Seq.choose (fun (idx, character) -> if character = '#' then None else Some idx) 
            |> Seq.toList

        (potentialIndexes, input |> Seq.skip 1)
        ||> Seq.fold (fun potentialIndexes line ->
            potentialIndexes
            |> List.filter (fun idx -> line[idx] <> '#')
        )

    input
    |> Array.indexed
    |> Array.collect (fun (rowIdx, line) ->
        line
        |> Seq.indexed
        |> Seq.choose (fun (columnIdx, character) ->
            if character = '#' then 
                let rowsToAdd = indexesOfEmptyRows |> Seq.takeWhile ((>) rowIdx) |> Seq.sumBy (fun _ -> emptyMultiplier)
                let columnsToAdd = indexesOfEmptyColumns |> Seq.takeWhile ((>) columnIdx) |> Seq.sumBy (fun _ -> emptyMultiplier)
                Some (bigint rowIdx + rowsToAdd, bigint columnIdx + columnsToAdd)
            else
                None
        )
        |> Seq.toArray
    )

let private calculteDistances (nodes: (bigint * bigint) array) =    
    nodes
    |> Array.indexed
    |> Array.sumBy (fun (index, (row, column)) ->
        nodes[index + 1..]
        |> Array.sumBy (fun (otherRow, otherColumn) ->
            abs (row - otherRow) +  abs (column - otherColumn)
        )
    )

let part1 (input) =
    parseInput input (bigint 1)
    |> calculteDistances

let part2 (input) =
    parseInput input (bigint 999_999)
    |> calculteDistances