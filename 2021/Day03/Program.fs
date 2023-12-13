/// https://adventofcode.com/2021/day/3
module Year2021Day3
open System

let part1 (input: string array) =
    let firstItem = input[0]
    let zero = firstItem |> Seq.map (fun _character -> 0)
    let nbElements = input.Length

    let foldedList =
        input
        |> Seq.fold (fun acc next ->
            let counted = next |> Seq.map (function '1' -> 1 | _ -> 0)
            Seq.map2 (fun x y -> x + y) acc counted
        ) zero

    let mostCommonBits =
        foldedList
        |> Seq.map (fun count -> if count < nbElements / 2 then "1" else "0")
        |> (fun x -> String.Join ("", x))
        |> (fun x -> Convert.ToInt32(x, 2))

    let leastCommonBits =
        foldedList
        |> Seq.map (fun count -> if count < nbElements / 2 then "0" else "1")
        |> (fun x -> String.Join ("", x))
        |> (fun x -> Convert.ToInt32(x, 2))

    mostCommonBits*leastCommonBits

let part2 (input: string array) =
    let countOccurencesOfOne (list: string list, index: int) =
        list
        |> Seq.sumBy (fun str -> if str[index] = '1' then 1 else 0)

    let oxygenGeneratorRating (list: string list) =
        let rec oxygenGeneratorRating (list: string list, index: int) =
            if list.Length = 1 then
                list.Head
            else
                let occurencesOfOne = countOccurencesOfOne (list, index)
                let mostCommonBit = if float occurencesOfOne < Math.Ceiling(float list.Length / 2.) then '0' else '1'
                oxygenGeneratorRating (list |> List.filter (fun str -> str[index] = mostCommonBit), index + 1)
        oxygenGeneratorRating (list, 0)

    let rec co2ScrubberRating (list: string list) =
        let rec co2ScrubberRating (list: string list, index: int) =
            if list.Length = 1 then
                list.Head
            else
                let occurencesOfOne = countOccurencesOfOne (list, index)
                let leastCommonBit = if float occurencesOfOne < Math.Ceiling(float list.Length / 2.) then '1' else '0'
                co2ScrubberRating (list |> List.filter (fun str -> str[index] = leastCommonBit), index + 1)
        co2ScrubberRating (list, 0)

    let inputList = input |> Seq.toList
    let oxyRating = Convert.ToInt32 (oxygenGeneratorRating inputList, 2)
    let co2Rating = Convert.ToInt32 (co2ScrubberRating inputList, 2)
    oxyRating*co2Rating