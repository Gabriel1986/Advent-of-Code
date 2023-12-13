/// https://adventofcode.com/2023/day/7
module Year2023Day7
open System.Text.RegularExpressions

let digitRegex = Regex(@"\d+")

let cards =
    [
        '2', 2
        '3', 3
        '4', 4
        '5', 5
        '6', 6
        '7', 7
        '8', 8
        '9', 9
        'T', 10
        'J', 11
        'Q', 12
        'K', 13
        'A', 14
    ]
    |> Map.ofList

type HandResult =
    | FiveOfAKind
    | FourOfAKind
    | FullHouse
    | ThreeOfAKind
    | TwoPairs
    | OnePair
    | HighCard

let determineResult =
    function
    | (_ , 5)::xs -> FiveOfAKind, 7
    | (_ , 4)::xs -> FourOfAKind, 6
    | (_ , 3)::(_ , 2)::xs -> FullHouse, 5
    | (_ , 3)::xs -> ThreeOfAKind, 4
    | (_ , 2)::(_ , 2)::xs -> TwoPairs, 3
    | (_ , 2)::xs -> OnePair, 2
    | _ -> HighCard, 1

let parseHandPart1 (hand: string) =
    let translatedHand = hand |> Seq.choose (fun card -> cards |> Map.tryFind card)
    let translatedHandAsString = translatedHand |> Seq.map (fun nbr -> nbr.ToString().PadLeft(2, '0'))
    let handAsInteger = int (String.joinWith "" translatedHandAsString)
    let (result, resultAsSortable) =
        translatedHand
        |> Seq.countBy id
        |> Seq.sortByDescending snd
        |> Seq.toList
        |> determineResult
    (resultAsSortable, handAsInteger)

let parseHandPart2 (hand: string) =
    let cardsForPart2 = cards |> Map.change 'J' (fun _ -> Some 0)

    let optimizeHand (cardValues: int seq) =
        let sortedCardValues =
            cardValues
            |> Seq.countBy id
            |> Seq.sortByDescending snd

        let nbJokers = sortedCardValues |> Seq.tryPick (fun (cardValue, nbOccurences: int) -> if cardValue = cardsForPart2['J'] then Some nbOccurences else None)

        match nbJokers with
        | None -> cardValues
        | Some nbJokers ->
            match sortedCardValues |> Seq.tryPick (fun (cardValue, nbOccurences: int) -> if cardValue <> cardsForPart2['J'] then Some cardValue else None) with
            | None -> cardValues
            | Some firstCardValueThatIsNotAJoker ->
                cardValues
                |> Seq.choose (fun cardValue -> if cardValue = cardsForPart2['J'] then None else Some cardValue)
                |> Seq.append (Seq.init nbJokers (fun _ -> firstCardValueThatIsNotAJoker))

    let translatedHand = hand |> Seq.choose (fun card -> cardsForPart2 |> Map.tryFind card)
    let translatedHandAsString = translatedHand |> Seq.map (fun nbr -> nbr.ToString().PadLeft(2, '0'))
    let handAsInteger = int (String.joinWith "" translatedHandAsString)
    let (result, resultAsSortable) =
        translatedHand
        |> optimizeHand
        |> Seq.countBy id
        |> Seq.sortByDescending snd
        |> Seq.toList
        |> determineResult
    (resultAsSortable, handAsInteger)

let parseInput (parseHand: string -> int * int) (input: string array) =
    input
    |> Array.map (fun line ->
        let splitLine = line.Split(" ")
        (parseHand (splitLine[0]), int (splitLine[1]))
    )

let part1 (input) =
    parseInput parseHandPart1 input
    |> Array.sortBy (fun ((handResult, comparableResult), _) -> handResult, comparableResult)
    |> Array.indexed
    |> Array.sumBy (fun (idx, (( _, _), bid)) -> (idx + 1) * bid)

let part2 (input) =
    parseInput parseHandPart2 input
    |> Array.sortBy (fun ((handResult, comparableResult), _) -> handResult, comparableResult)
    |> Array.indexed
    |> Array.sumBy (fun (idx, (( _, _), bid)) -> (idx + 1) * bid)