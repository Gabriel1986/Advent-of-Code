/// https://adventofcode.com/2022/day/2
module Year2022Day2

let dictionary =
    [
        "A", "X"
        "B", "Y"
        "C", "Z"
    ]
    |> Map.ofList

let beatsDictionary =
    [
        "X", "Z"
        "Y", "X"
        "Z", "Y"
    ]
    |> Map.ofList

let losesDictionary =
    [
        "X", "Y"
        "Y", "Z"
        "Z", "X"
    ]
    |> Map.ofList

let playedItemScore =
    [
        "X", 1
        "Y", 2
        "Z", 3
    ]
    |> Map.ofList


let part1 (input: string array) =
    let getScore (a: string) (b: string) =
        let convertedA = dictionary[a]
        let matchScore =
            if (convertedA = b) then
                3
            elif beatsDictionary[convertedA] = b then
                0
            else
                6

        playedItemScore[b] + matchScore

    input
    |> Array.map (fun x -> x.Split(' '))
    |> Array.sumBy (fun x -> getScore x[0] x[1])

let part2 (input: string array) =
    let getScore (a: string) (b: string) =
        let convertedA = dictionary[a]

        let convertedB =
            if b = "X" then
                beatsDictionary[convertedA]
            elif b = "Y" then
                convertedA
            else
                losesDictionary[convertedA]

        let matchScore =
            if (convertedA = convertedB) then
                3
            elif beatsDictionary[convertedA] = convertedB then
                0
            else
                6

        playedItemScore[convertedB] + matchScore

    input
    |> Array.map (fun x -> x.Split(' '))
    |> Array.sumBy (fun x -> getScore x[0] x[1])