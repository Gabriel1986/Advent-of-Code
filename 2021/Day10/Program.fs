/// https://adventofcode.com/2021/day/10
module Year2021Day10

let part1 (input: string array) =
    let validClosingMap = Map [
        '[', ']'
        '<', '>'
        '(', ')'
        '{', '}'
    ]

    let pointMap = Map [
        ')', 3
        ']', 57
        '}', 1197
        '>', 25137
    ]

    let findSyntaxError (line: string) =
        line
        |> Seq.fold (fun (stackOfOpenCharacters: char list, syntaxError: int option) (nextCharacter: char) ->
            match syntaxError with
            | Some syntaxError -> (stackOfOpenCharacters, Some syntaxError)
            | None ->
                if validClosingMap.ContainsKey nextCharacter then
                    (nextCharacter::stackOfOpenCharacters, syntaxError)
                else
                    match stackOfOpenCharacters with
                    | [] ->
                        (stackOfOpenCharacters, Some (pointMap[nextCharacter]))
                    | head::tail when validClosingMap[head] = nextCharacter ->
                        (tail, None)
                    | _ ->
                        ([], Some (pointMap[nextCharacter]))) ([], None)
        |> snd

    input
    |> Seq.choose findSyntaxError
    |> Seq.sum

let part2 (input: string array) =
    let validClosingMap = Map [
        '[', ']'
        '<', '>'
        '(', ')'
        '{', '}'
    ]

    let pointMap = Map [
        ')', 1L
        ']', 2L
        '}', 3L
        '>', 4L
    ]

    let findOpenCharacters (line: string) =
        line
        |> Seq.fold (fun (stackOfOpenCharacters: char list, hasSyntaxError: bool) (nextCharacter: char) ->
            match hasSyntaxError with
            | true -> (stackOfOpenCharacters, true)
            | false ->
                if validClosingMap.ContainsKey nextCharacter then
                    (nextCharacter::stackOfOpenCharacters, false)
                else
                    match stackOfOpenCharacters with
                    | [] ->
                        (stackOfOpenCharacters, true)
                    | head::tail when validClosingMap[head] = nextCharacter ->
                        (tail, false)
                    | _ ->
                        ([], true)) ([], false)
        |> (fun (openCharacters: char list, syntaxError: bool) ->
            match syntaxError with
            | true -> None
            | false -> Some openCharacters
        )

    let scores =
        input
        |> Array.choose (findOpenCharacters >> Option.map (List.fold (fun totalScore character -> 5L * totalScore + pointMap[validClosingMap[character]]) 0L))

    scores
    |> Array.sort
    |> Array.item (scores.Length / 2)
    |> bigint