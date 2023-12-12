/// https://adventofcode.com/2021/day/8
module Year2021Day8
open System

type Word = {
    Characters: Set<char>
    Length: int
}

module Set =
    let unsafeUniqueElement (set: Set<char>) =
        if set.Count <> 1 then failwithf "I made booboo" else set.MinimumElement

let part1 (input: string array) =
    let justDigits =
        [
            for line in input do
                match line.Split("|") with
                | [| _; toFind |] ->
                    let parseWord (str: string) =
                        let word = str.Trim()
                        {
                            Characters = Set.ofSeq word
                            Length = word.Length
                        }
                    toFind.Trim().Split(" ") |> Array.map parseWord
                | other -> failwithf "Day 8 - Part 1 - Parsing error..."
        ]

    justDigits
    |> List.sumBy (fun (digitsToFind) ->
        digitsToFind
        |> Array.sumBy (fun word -> if List.contains word.Length [ 2; 3; 4; 7 ] then 1 else 0)
    )

let part2 (input: string array) =
    let parsedInput = [
        for line in input do
            match line.Split("|") with
            | [| givenDigitsString; digitsToFindString |] ->
                let parseString (str: string) =
                    str.Trim().Split(" ")
                    |> Array.map (fun x -> Set.ofSeq (x.Trim()))

                {|
                    GivenDigits = parseString givenDigitsString
                    DigitsToFind = parseString digitsToFindString
                |}
            | other -> failwithf "Day 8 - Part 2 - Parsing error..."
    ]

    let findWordOfLength (words: Set<char>[], nbCharacters: int) =
        words |> Array.find (fun w -> w.Count = nbCharacters)

    let occurencesOfCharacter (words: Set<char>[], character: char) =
        words |> Array.sumBy (fun word -> if word.Contains character then 1 else 0)

    let createTranslationDictionary (wordList: Set<char>[]) =
        let number1 = findWordOfLength (wordList, 2)
        let number7 = findWordOfLength (wordList, 3)
        let number4 = findWordOfLength (wordList, 4)
        let number8 = findWordOfLength (wordList, 7)

        let occurencesOfCharacter = [ 'a'..'g' ] |> List.map (fun each -> (each, occurencesOfCharacter (wordList, each)))
        let bottomRight = occurencesOfCharacter |> List.pick (fun (character, occurences) -> if occurences = 9 then Some character else None)
        let bottomLeft  = occurencesOfCharacter |> List.pick (fun (character, occurences) -> if occurences = 4 then Some character else None)
        let topLeft     = occurencesOfCharacter |> List.pick (fun (character, occurences) -> if occurences = 6 then Some character else None)
        let topRight    = Set.unsafeUniqueElement (number1 - Set [ bottomRight ])
        let middle      = Set.unsafeUniqueElement (number4 - Set [ topRight; bottomRight; topLeft ])
        let top         = Set.unsafeUniqueElement (number7 - number1)
        let bottom      = Set.unsafeUniqueElement (number8 - Set [ top; bottomRight; bottomLeft; topLeft; topRight; middle ])

        Map [
            (Set [ top; topRight; topLeft; bottomLeft; bottomRight; bottom ], "0")
            (number1, "1")
            (Set [ top; topRight; middle; bottomLeft; bottom ], "2")
            (Set [ top; topRight; middle; bottomRight; bottom ], "3")
            (number4, "4")
            (Set [ top; topLeft; middle; bottomRight; bottom ], "5")
            (Set [ top; topLeft; middle; bottomRight; bottomLeft; bottom ], "6")
            (number7, "7")
            (number8, "8")
            (Set [ top; topLeft; topRight; middle; bottomRight; bottom ], "9")
        ]

    parsedInput
    |> List.sumBy (fun inputPart ->
        let givenDictionary = createTranslationDictionary inputPart.GivenDigits

        inputPart.DigitsToFind
        |> Array.map (fun digit -> givenDictionary |> Map.find digit)
        |> String.concat ""
        |> Int32.Parse
    )