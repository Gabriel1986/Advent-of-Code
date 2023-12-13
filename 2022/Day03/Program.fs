/// https://adventofcode.com/2022/day/3
module Year2022Day3
open System

let part1 (input: string array) =
    input
    |> Array.sumBy (fun line ->
        let firstHalf = line.Substring(0, line.Length / 2)
        let secondHalf = line.Substring(line.Length / 2)
        let commonCharacter = firstHalf |> Seq.pick (fun x -> if secondHalf.Contains(x) then Some(x) else None)
        if Char.IsLower commonCharacter then
            int commonCharacter - int 'a' + 1
        else
            int commonCharacter - int 'A' + 27
    )

let part2 (input: string array) =
    input
    |> Array.chunkBySize 3
    |> Array.sumBy (fun line ->
        let commonCharacter = line[0] |> Seq.pick (fun x -> if line[1].Contains(x) && line[2].Contains(x) then Some(x) else None)
        if Char.IsLower commonCharacter then
            int commonCharacter - int 'a' + 1
        else
            int commonCharacter - int 'A' + 27
    )