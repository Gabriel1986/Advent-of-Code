﻿[<AutoOpen>]
module Helpers
    open System
    open System.Collections.Generic
    open System.Text.RegularExpressions

    module Regex =
        let numberRegex = Regex("-?\d+", RegexOptions.Compiled)

    module Math =
        let BetweenInclusive (lowerBound: int) (upperBound: int) (value: int) =
            value >= lowerBound && value <= upperBound

        let Between (lowerBound: int) (upperBound: int) (value: int) =
            value > lowerBound && value < upperBound


    module String =
        let joinWith (separator: string) (collectionOfStrings: IEnumerable<string>) =
            String.Join (separator, collectionOfStrings)

    module Seq =
        let multiply = Seq.fold (*) 1
        let multiplyBy (projection: 'T -> int) = Seq.map projection >> multiply

    module List =
        let take' n (list: 'a list) =
            if n > list.Length then
                list
            else
                list |> List.take n

        let safeMaxBy (predicate: 'a -> bool) (list: 'a list) =
            if list |> List.exists predicate then
                Some (list |> List.maxBy predicate)
            else
                None


        let rec permute (list: 'a list) : seq<'a list> = 
            seq {
                match list with
                | [] -> yield []
                | _ ->
                    for i in 0 .. List.length list - 1 do
                        let x = list.[i]
                        // Exclude element at index i to get the rest of the list
                        let rest = list.[..i-1] @ list.[i+1..]
                        // Recursively get permutations of the rest
                        for perm in permute rest do
                            yield x :: perm
            }

    let private binaryConversionMap = Map [
        '0', "0000"
        '1', "0001"
        '2', "0010"
        '3', "0011"
        '4', "0100"
        '5', "0101"
        '6', "0110"
        '7', "0111"
        '8', "1000"
        '9', "1001"
        'A', "1010"
        'B', "1011"
        'C', "1100"
        'D', "1101"
        'E', "1110"
        'F', "1111"
    ]

    let convertToBinary (character: char) =
        binaryConversionMap[character]