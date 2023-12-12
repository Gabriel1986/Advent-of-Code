/// https://adventofcode.com/2023/day/4
module Year2023Day4
open System.Text.RegularExpressions

let numberRegex = Regex(@"\d+")

let calculateNbMatchesOnLine (line: string) =
    match (line.Split(":")[1]).Split("|") with
    | [| winningNumbers; myNumbers |] ->
        let winningNumbers = numberRegex.Matches(winningNumbers) |> Seq.map (fun m -> int m.Value)
        let myNumbers = numberRegex.Matches(myNumbers) |> Seq.map (fun m -> int m.Value)

        winningNumbers
        |> Seq.sumBy (fun n -> if myNumbers |> Seq.exists (fun m -> n = m) then 1 else 0)
    | _ -> failwith "Invalid input"

let part1 (input) =
    let calculatationFn nbMatches =
        if nbMatches < 2 then nbMatches else int (pown 2 (nbMatches - 1))

    input
    |> Array.sumBy (calculateNbMatchesOnLine >> calculatationFn)

let part2 (input) =
    let mbMatchesOnLines =
        input
        |> Array.map calculateNbMatchesOnLine
        |> Array.indexed

    let initialNbScratchCardsArray = Array.create mbMatchesOnLines.Length 1

    (initialNbScratchCardsArray, mbMatchesOnLines)
    ||> Array.fold (fun nbScratchCardsArray (lineIdx, nbMatchesOnLine) ->
        match nbMatchesOnLine with
        | 0 -> nbScratchCardsArray
        | _ ->
            let amountOfScratchCardsToAdd = nbScratchCardsArray[lineIdx]

            nbScratchCardsArray 
            |> Array.mapi (fun idx nbScratchCardsAtIdx ->
                if idx >= lineIdx + 1 && idx <= lineIdx + nbMatchesOnLine then
                    nbScratchCardsAtIdx + amountOfScratchCardsToAdd
                else
                    nbScratchCardsAtIdx
            )
    )
    |> Array.sum