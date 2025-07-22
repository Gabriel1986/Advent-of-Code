/// https://adventofcode.com/2015/day/11
module Year2015Day11

open System
open System.Linq

let forbiddenCharacters = [| int 'i'; int 'o'; int 'l' |]
let a = int 'a'
let z = int 'z'

let inline resetArrayAfter (array: int[]) (index: int) =
    for clearingIndex = index + 1 to array.Length - 1 do
        array[clearingIndex] <- a

let inline increment (array: int[]) =
    let rec incrementRec (index: int) =
        let numberToIncrement = array[index]
        if numberToIncrement = z then
            array[index] <- a
            incrementRec (index-1)
        else
            let nextCharacter = numberToIncrement + 1
            if forbiddenCharacters.Contains(nextCharacter) then
                resetArrayAfter array index
                array[index] <- nextCharacter + 1
            else
                array[index] <- numberToIncrement + 1
    incrementRec (array.Length - 1)

let rec countPairs (arr: int[]) (currentIndex: int) (lastPairIdx: int) (currentCount: int) =
    if currentIndex >= arr.Length then 
        currentCount
    else
        if currentIndex > 0 && arr[currentIndex] = arr[currentIndex-1] && (currentIndex < 2 || currentIndex - 2 <> lastPairIdx) then
            countPairs arr (currentIndex + 1) (currentIndex - 1) (currentCount + 1)
        else
            countPairs arr (currentIndex + 1) lastPairIdx currentCount

let runAlgorithm (input: int[]) =
    let applyRule1 (arr: _[]) =
        let mutable i = 2
        let mutable found = false
        while i < arr.Length && not found do
            found <- arr[i] = arr[i - 1] + 1 && arr[i - 1] = arr[i - 2] + 1
            i <- i + 1
        found

    let applyRule2 (arr: int[]) =
        arr
        |> Array.exists (fun ch -> forbiddenCharacters.Contains(ch))
        |> not

    let applyRule3 (arr: int[]) =
        countPairs arr 1 -1 0 
        |> fun c -> c >= 2

    if input |> Array.exists (fun ch -> forbiddenCharacters.Contains(ch)) then
        let idx = input |> Array.findIndex (fun ch -> forbiddenCharacters.Contains(ch))
        do resetArrayAfter input idx
        input[idx] <- input[idx] + 1

    let rec findNextPassword (array: int[]) =
        do increment array

        let hasFinalResult =
            [ applyRule1; applyRule2; applyRule3 ]
            |> List.forall (fun fn -> fn array)

        //printfn "Intermediate result: %A" (new string(array.Select(char).ToArray()))

        if hasFinalResult then
            array
        else        
            findNextPassword array

    findNextPassword input

let part1 (input: string[]) =
    let parsed = input[0].ToCharArray().Select(int).ToArray()
    let result = runAlgorithm parsed
    new string(result.Select(char).ToArray())

let part2 (input: string[]) =
    let parsed = input[0].ToCharArray().Select(int).ToArray()
    let result = runAlgorithm parsed
    increment result
    let newResult = runAlgorithm result
    new string(newResult.Select(char).ToArray())