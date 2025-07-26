/// https://adventofcode.com/2015/day/18
module Year2015Day18

let inline private parseLights (gridSize) (input: string[]) =
    let lightsArray = Array.zeroCreate<bool> (gridSize * gridSize)
    for x = 0 to gridSize - 1 do
        for y = 0 to gridSize - 1 do
            match input[x][y] with
            | '#' -> lightsArray[x * gridSize + y] <- true
            | _ -> ()
    lightsArray

let inline private countNeighbourLights (gridSize: int) (x: int, y: int) (state: bool[]) =
    let mutable count = 0
    let idx = x * gridSize + y
    let max = gridSize - 1

    let inline check xi yi =
        if xi >= 0 && xi <= max && yi >= 0 && yi <= max && (xi <> x || yi <> y) then
            if state[xi * gridSize + yi] then
                count <- count + 1

    check (x-1) (y-1)
    check (x-1) y
    check (x-1) (y+1)
    check x (y-1)
    if count < 4 then check x (y+1)
    if count < 4 then check (x+1) (y-1)
    if count < 4 then check (x+1) y
    if count < 4 then check (x+1) (y+1)

    count
    

let inline private determineLightShouldStayOn (gridSize: int) (currentState: bool[]) (x: int, y: int) =
    currentState
    |> countNeighbourLights gridSize (x, y) 
    |> fun count -> count = 3 || count = 2

let inline private determineLightShouldTurnOn (gridSize: int) (currentState:  bool[]) (x: int, y: int) =
    currentState
    |> countNeighbourLights gridSize (x, y)
    |> (=) 3

let inline private evolve (gridSize: int) (currentState: bool[]) =
    let next = Array.zeroCreate<bool> (gridSize * gridSize)
    for x in 0 .. gridSize - 1 do
        for y in 0 .. gridSize - 1 do
            let idx = x * gridSize + y
            let neighboursOn = countNeighbourLights gridSize (x, y) currentState
            if currentState[idx] then
                next[idx] <- neighboursOn = 3 || neighboursOn = 2
            else
                next[idx] <- neighboursOn = 3
    next

let runPart1 (input: string[]) (nbIterations: int) =
    let gridSize = input.Length
    let mutable result = parseLights gridSize input
    for i = 1 to nbIterations do
        result <- evolve gridSize result
        
    let mutable count = 0
    for i in 0 .. result.Length - 1 do
        if result[i] then count <- count + 1
    count

let inline private turnCornerLightsOn (gridSize: int) (input: bool[]) =
    let last = gridSize - 1
    input[0] <- true
    input[last] <- true
    input[last * gridSize] <- true
    input[last * gridSize + last] <- true

let runPart2 (input: string[]) (nbIterations: int) =
    let gridSize = input.Length
    let mutable result = parseLights gridSize input
    turnCornerLightsOn gridSize result
    
    for i = 1 to nbIterations do
        result <- evolve gridSize result
        turnCornerLightsOn gridSize result
        
    let mutable count = 0
    for i in 0 .. result.Length - 1 do
        if result[i] then count <- count + 1
    count

let testPart1 (input: string[]) =
    runPart1 input 4

let part1 (input: string[]) =
    runPart1 input 100

let testPart2 (input: string[]) =
    runPart2 input 5

let part2 (input: string[]) =
    runPart2 input 100
