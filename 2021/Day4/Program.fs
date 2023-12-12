/// https://adventofcode.com/2021/day/4
module Year2021Day4
open System

type RowNumber = { Number: int; Selected: bool }
type Board =
    Board of RowNumber array list
        member me.HasBingo () =
            let (Board board) = me

            let everythingInRowSelected () = board |> List.exists (fun row -> row |> Array.forall (fun row -> row.Selected))
            let everythingInColumnSelected () =
                let rec everythingInColumnSelected (board: RowNumber array list, index: int) =
                    if board |> List.forall (fun row -> row[index].Selected) then
                        true
                    elif index < board[0].Length - 1 then
                        everythingInColumnSelected (board, index + 1)
                    else
                        false
                everythingInColumnSelected (board, 0)

            everythingInRowSelected () || everythingInColumnSelected ()

        member me.MarkNumber (number: int) =
            let (Board board) = me

            board
            |> List.map (fun row -> row |> Array.map (fun rowNumber -> if rowNumber.Number = number then { rowNumber with Selected = true } else rowNumber))
            |> Board

let private parseInput (input: string array) =
    let parseRow (str: string) =
        str.Split(" ")
        |> Array.filter ((<>) "")
        |> Array.map (fun each -> { Number = int (each.Trim()); Selected = false })

    let bingoGame =
        input[0].Split(",")
        |> Array.map (fun each -> int (each.Trim()))
        |> List.ofArray

    let boards =
        input[2..]
        |> Array.fold (fun (acc: Board list) next ->
            if next = "" then
                [ Board []; yield! acc ]
            else
                match acc with
                | [] ->
                    [ Board [ parseRow next ] ]
                | (Board head)::tail ->
                    (Board (parseRow next::head))::tail
        ) ([]: Board list)

    bingoGame, boards

let part1 (input) =
    let rec tryFindBoardWithBingo (bingoGame: int list, boards: Board list) =
        match bingoGame with
        | [] ->
            None
        | head::tail ->
            let newBoards = boards |> List.map (fun board -> board.MarkNumber head)
            match newBoards |> List.tryFind (fun board -> board.HasBingo()) with
            | Some board -> Some (board, head)
            | None -> tryFindBoardWithBingo (tail, newBoards)

    match tryFindBoardWithBingo (parseInput input) with
    | Some (Board board, lastNumber) ->
        let sumOfUnselected = board |> List.sumBy (fun row -> row |> Array.sumBy (fun number -> if number.Selected then 0 else number.Number))
        sumOfUnselected * lastNumber
    | None ->
        0

let part2 (input) =
    let rec tryFindLastBoardWithBingo (bingoGame: int list, boards: Board list) =
        match boards, bingoGame with
        | _, [] | [], _ ->
            None
        | [ singleBoard ], head::tail ->
            let newBoard = singleBoard.MarkNumber head
            if newBoard.HasBingo () then
                Some (newBoard, head)
            else
                tryFindLastBoardWithBingo (tail, [ singleBoard ])
        | manyBoards, head::tail ->
            let newBoards = manyBoards |> List.map (fun board -> board.MarkNumber head)
            tryFindLastBoardWithBingo (tail, newBoards |> List.filter (fun board -> not (board.HasBingo ())))

    match tryFindLastBoardWithBingo (parseInput input) with
    | Some (Board board, lastNumber) ->
        let sumOfUnselected =
            board
            |> List.sumBy (fun row -> row |> Array.sumBy (fun number -> if number.Selected then 0 else number.Number))
        sumOfUnselected * lastNumber
    | None ->
        0