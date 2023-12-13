/// https://adventofcode.com/2022/day/1
module Year2022Day1

type SingleMax = { CurrentMax: int; CurrentSum: int }

let part1 (input) =
    ({ CurrentMax = 0; CurrentSum = 0 }, input)
    ||> Array.fold (fun acc next ->
        if next = "" then
            { CurrentMax = max acc.CurrentMax acc.CurrentSum; CurrentSum = 0 }
        else
            { CurrentMax = acc.CurrentMax; CurrentSum = acc.CurrentSum + int next }
    )
    |> fun x -> x.CurrentMax

type Maxes = { CurrentMaxes: int list; CurrentSum: int }

let part2 (input) =
    ({ CurrentMaxes = []; CurrentSum = 0 }, input)
    ||> Array.fold (fun acc next ->
        if next = "" then
            { CurrentSum = 0; CurrentMaxes = List.append [acc.CurrentSum] acc.CurrentMaxes |> List.sortDescending |> List.take' 3 }
        else
            { acc with CurrentSum = acc.CurrentSum + int next }
    )
    |> fun x -> x.CurrentMaxes |> List.sum