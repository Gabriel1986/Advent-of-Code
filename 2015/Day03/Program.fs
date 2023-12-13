module Year2015Day3

type Move =
    | Up
    | Down
    | Left
    | Right
    static member Parse =
        function '^' -> Up | 'v' -> Down | '<' -> Left | '>' -> Right | other -> failwithf "Invalid character '%c'" other

let parseInput (input: string array) =
    input[0]
    |> Seq.map Move.Parse

type Point =
    {
        X: int
        Y: int
    }
    member me.ExecuteMove =
        function
        | Up -> { me with Y = me.Y + 1 }
        | Down -> { me with Y = me.Y - 1 }
        | Right -> { me with X = me.X + 1 }
        | Left -> { me with X = me.X - 1 }

let part1 (input) =
    let startingPoint = { X = 0; Y = 0 }
    let mappedLocations =
        parseInput input
        |> Seq.fold (fun (currentLocation: Point, map: Map<Point, int>) (nextMove: Move) ->
            let newLocation = currentLocation.ExecuteMove nextMove
            let newMap = map |> Map.change newLocation (function Some value -> Some (value + 1) | None -> Some 1)
            (newLocation, newMap)
        ) (startingPoint, Map [ (startingPoint, 1) ])

    mappedLocations
    |> snd
    |> Map.count

let part2 (input) =
    let startingPoint = { X = 0; Y = 0 }
    let mappedLocations =
        parseInput input
        |> Seq.indexed
        |> Seq.fold (fun (santaLocation: Point, roboSantaLocation: Point, map: Map<Point, int>) (index: int, move: Move) ->
            if index % 2 = 0 then
                let newSantaLocation = santaLocation.ExecuteMove move
                let newMap = map |> Map.change newSantaLocation (function Some value -> Some (value + 1) | None -> Some 1)
                (newSantaLocation, roboSantaLocation, newMap)
            else
                let newRoboSantaLocation = roboSantaLocation.ExecuteMove move
                let newMap = map |> Map.change newRoboSantaLocation (function Some value -> Some (value + 1) | None -> Some 1)
                (santaLocation, newRoboSantaLocation, newMap)
        ) (startingPoint, startingPoint, Map [ (startingPoint, 2) ])

    mappedLocations
    |> (fun (_, _, map) -> map)
    |> Map.count