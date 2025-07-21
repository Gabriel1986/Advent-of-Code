/// https://adventofcode.com/2015/day/1
module Year2015Day1

let characterToFloorMovement =
    function
    | '(' -> 1
    | ')' -> -1
    | other -> failwithf "Unknown character: %c" other

let part1 (input: string array) =
    input[0]
    |> Seq.sumBy characterToFloorMovement

// Taking advantage that sequences are lazy, find will only calculate the sequence until it reaches the first hit
let part2 (input: string array) =
    let input = input[0]

    seq {
        let mutable currentFloor = 0
        for i in 0..input.Length-1 do
            currentFloor <- currentFloor + characterToFloorMovement (input[i])
            yield (i + 1, currentFloor)
    }
    |> Seq.find (fun (_, floor) -> floor = -1)
    |> fst