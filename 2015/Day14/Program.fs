/// https://adventofcode.com/2015/day/14
module Year2015Day14

let numberRegex = Regex.numberRegex
let parse (input: string) =
    let matches = numberRegex.Matches(input)
    (int matches[0].Value, int matches[1].Value, int matches[2].Value)

let inline calculateDistance (time: int) (speed, fly, sleep) =
    let iterationTime = fly + sleep
    let nbIterations = time / iterationTime
    let leftoverTime = time % iterationTime
    nbIterations * fly * speed + min leftoverTime fly * speed

let runPart1Algorithm (totalTime: int) (raindeer: (int * int * int) array) =
    (0, raindeer)
    ||> Array.fold (fun maxDistance raindeer ->
        max maxDistance (calculateDistance totalTime raindeer))

//Brute force attempt
let runPart2Algorithm (totalTime: int) (raindeer: (int * int * int) array) =
    let indexedRaindeer = raindeer |> Array.sortBy (fun (speed, _, _) -> speed) |> Array.indexed
    let raindeerScores = Array.init raindeer.Length (fun _ -> 0)

    for currentTime = 1 to totalTime + 1 do
        let currentLeadIndex, _, _ =
            ((0, 0, 0), indexedRaindeer)
            ||> Array.fold (fun (maxIndex, maxDistance, maxSpeed) (index, (speed, fly, sleep)) ->
                let totalDistance = calculateDistance currentTime (speed, fly, sleep)
                //Raindeer are ordered by speed, if the distances are equal, the fastest one wins
                if totalDistance >= maxDistance then
                    index, totalDistance, speed
                else
                    maxIndex, maxDistance, maxSpeed)
        raindeerScores[currentLeadIndex] <- raindeerScores[currentLeadIndex] + 1

    raindeerScores |> Array.max

let testPart1 (input: string[]) =
    runPart1Algorithm 1000 (input |> Array.map parse)

let part1 (input: string[]) =
    runPart1Algorithm 2503 (input |> Array.map parse)

let testPart2 (input: string[]) =
    runPart2Algorithm 1000 (input |> Array.map parse)

let part2 (input: string[]) =
    runPart2Algorithm 2503 (input |> Array.map parse)