/// https://adventofcode.com/2015/day/17
module Year2015Day17

let runPart1 (finalSum: int) (containers: int[]) =
    let rec loop (startingIdx: int) (sum: int) =
        seq {
            for i = startingIdx to containers.Length - 1 do
                let newSum = sum + containers[i]
                if newSum = finalSum then
                    yield 1
                elif newSum < finalSum then
                    yield! loop (i + 1) newSum
        }
    loop 0 0
    |> Seq.sum

let runPart2 (finalSum: int) (containers: int[]) =
    let rec loop (startingIdx: int) (sum: int) (depth: int) =
        //Find all of the sums that, at this depth match the final sum
        let matchesAtThisDepth =
            seq {
                for i = startingIdx to containers.Length - 1 do
                    if finalSum = sum + containers[i] then
                        yield depth
            }

        //If there is at least one match at this depth, skip the rest of the containers
        if matchesAtThisDepth |> Seq.isEmpty |> not then
            matchesAtThisDepth

        //Else we need to take one more container
        else
            seq {
                for i = startingIdx to containers.Length - 1 do
                    let newSum = sum + containers[i]
                    if newSum < finalSum then
                        yield! loop (i + 1) newSum (depth + 1)
            }

    let finalResults = loop 0 0 0 |> Seq.toList
    let minLevel = finalResults |> List.min

    finalResults
    |> List.sumBy (fun x -> if x = minLevel then 1 else 0)

let testPart1 (input: string[]) =
    input
    |> Array.map int
    |> runPart1 25

let part1 (input: string[]) =
    input
    |> Array.map int
    |> runPart1 150

let testPart2 (input: string[]) =
    input
    |> Array.map int
    |> runPart2 25

let part2 (input: string[]) =
    input
    |> Array.map int
    |> runPart2 150