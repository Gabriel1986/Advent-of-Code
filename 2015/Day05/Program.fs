/// https://adventofcode.com/2015/day/5
module Year2015Day5

let inline isVowel (character: char) =
    "aeiou".Contains(character)

let notAllowedCharacters =
    [
        ('a', 'b')
        ('c', 'd')
        ('p', 'q')
        ('x', 'y')
    ]

let determineNaughtyOrNicePart1 (str: string) =
    let characters: char[] = str.ToCharArray()
    let startingCharacter = characters[0]

    let initialState =
        {|
            NbVowels = if isVowel startingCharacter then 1 else 0
            HasDoubleCharacter = false
            Stopped = false
            CurrentCharacter = startingCharacter
        |}

    let acc =
        (initialState, characters[1..])
        ||> Array.fold (fun acc nextCharacter ->
            if acc.Stopped then
                acc
            else
                {|
                    NbVowels = acc.NbVowels + if isVowel nextCharacter then 1 else 0
                    HasDoubleCharacter = acc.HasDoubleCharacter || acc.CurrentCharacter = nextCharacter
                    CurrentCharacter = nextCharacter
                    Stopped = notAllowedCharacters |> List.contains((acc.CurrentCharacter, nextCharacter))
                |})

    if acc.Stopped || acc.NbVowels < 3 || not acc.HasDoubleCharacter then
        0
    else
        1

let part1 (input: string[]) =
    input
    |> Array.sumBy determineNaughtyOrNicePart1

let hasRepeatingPairWithoutOverlap (chars: char[]) =
    let rec loop (currentIdx: int) (alreadySeen: ((char * char) * int) list) =
        if currentIdx >= chars.Length - 1 then 
            false
        else
            let nextPair = chars[currentIdx], chars[currentIdx + 1]
            match alreadySeen |> List.tryPick (fun (seenPair, seenIdx) -> if seenPair = nextPair then Some seenIdx else None) with
            | Some seenIdx when seenIdx <= currentIdx - 2 ->
                true
            | Some _ ->
                loop (currentIdx + 1) alreadySeen
            | None ->
                loop (currentIdx + 1) ((nextPair, currentIdx) :: alreadySeen)

    loop 0 []

let hasRepeatingLetterWithOneBetween (chars: char[]) =
    chars
    |> Array.indexed
    |> Array.exists (fun (i, c) ->
        i <= chars.Length - 3 && c = chars[i + 2]
    )

let determineNaughtyOrNicePart2 (str: string) =
    let chars = str.ToCharArray()
    let skipRepeats () = hasRepeatingLetterWithOneBetween chars
    let pairRepeats () = hasRepeatingPairWithoutOverlap chars
    if skipRepeats () && pairRepeats () then 1 else 0

let part2 (input: string[]) =
    input
    |> Array.sumBy determineNaughtyOrNicePart2