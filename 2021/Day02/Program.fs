/// https://adventofcode.com/2021/day/2
module Year2021Day2

type Direction =
    | Forward of int
    | Up of int
    | Down of int

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some (int (s.Substring(p.Length).Trim()))
    else
        None

let private parseInput (input: string seq) =
    let parseDirection (str: string) =
        match str with
        | Prefix "forward" number -> Forward number
        | Prefix "up" number -> Up number
        | Prefix "down" number -> Down number
        | _ -> failwithf "Unknown direction."

    input
    |> Seq.map parseDirection

let part1 (input) =
    input
    |> parseInput
    |> Seq.fold (fun (h, v) direction ->
        match direction with
        | Up x -> (h, v-x)
        | Down x -> (h, v+x)
        | Forward x -> (h+x, v)
    ) (0, 0)
    |> fun (h, v) -> (h * v)

let part2 (input: string seq) =
    input
    |> parseInput
    |> Seq.fold (fun (h, v, aim) direction ->
        match direction with
        | Up x -> (h, v, aim - x)
        | Down x -> (h, v, aim + x)
        | Forward x -> (h + x, v + x*aim, aim)
    ) (0, 0, 0)
    |> fun (h, v, _) -> (h * v)