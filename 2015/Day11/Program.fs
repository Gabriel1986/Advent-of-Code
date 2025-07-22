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

let inline incrementPassword (array: int[]) =
    let rec loop (index: int) =
        let numberToIncrement = array[index]
        if numberToIncrement = z then
            array[index] <- a
            loop (index-1)
        else
            let nextCharacter = numberToIncrement + 1
            if forbiddenCharacters.Contains(nextCharacter) then
                resetArrayAfter array index
                array[index] <- nextCharacter + 1
            else
                array[index] <- numberToIncrement + 1
    loop (array.Length - 1)

let runAlgorithm (input: int[]) =
    //Clear up the input
    if input |> Array.exists (fun ch -> forbiddenCharacters.Contains(ch)) then
        let index = input |> Array.findIndex (fun ch -> forbiddenCharacters.Contains(ch))
        do resetArrayAfter input index
        input[index] <- input[index] + 1

    let inline hasIncrementalCharacters (arr: _[]) =
        let rec loop index =
            if index >= arr.Length then 
                false
            elif arr[index - 2] + 1 = arr[index - 1] && arr[index - 1] + 1 = arr[index] then 
                true
            else 
                loop (index + 1)
        if arr.Length < 3 then 
            false 
        else
            loop 2

    let inline hasTwoDuplicates (arr: int[]) =
        let rec loop index pairs =
            if index >= arr.Length then 
                false
            elif arr[index] = arr[index - 1] then
                let newPairs = pairs + 1
                if newPairs = 2 then 
                    true
                else 
                    loop (index + 2) newPairs
            else
                loop (index + 1) pairs
        loop 1 0

    let rec findNextPassword (array: int[]) =
        do incrementPassword array

        if hasIncrementalCharacters array && hasTwoDuplicates array then
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
    incrementPassword result
    let newResult = runAlgorithm result
    new string(newResult.Select(char).ToArray())