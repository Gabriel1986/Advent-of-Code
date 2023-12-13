/// https://adventofcode.com/2021/day/22
module Year2021Day22
open System.Text.RegularExpressions

type AddOrSubtract =
    | Add
    | Subtract
    member this.reverse() =
        match this with
        | Add -> Subtract
        | Subtract -> Add
    member this.multiply() =
        match this with
        | Add -> bigint 1
        | Subtract -> bigint -1

type Cuboid = { Type: AddOrSubtract; X: int * int; Y: int * int; Z: int * int }
let private digitRegex = Regex(@"-?\d+")

let private hasOverlap (cuboid1: Cuboid) (cuboid2: Cuboid) =
    let hasOverlap (range1: int * int) (range2: int * int) =
        let (min1, max1) = range1
        let (min2, max2) = range2
        min1 <= max2 && min2 <= max1

    hasOverlap cuboid1.X cuboid2.X && hasOverlap cuboid1.Y cuboid2.Y && hasOverlap cuboid1.Z cuboid2.Z

//Calculates an overlap betwwen two cuboids
let private calculateOverlapBetween (cuboid1: Cuboid) (cuboid2: Cuboid): Cuboid option =
    if hasOverlap cuboid1 cuboid2 then
        let calculateOverlap (range1: int * int) (range2: int * int) =
            let (min1, max1) = range1
            let (min2, max2) = range2
            max min1 min2, min max1 max2

        Some {
            Type = cuboid2.Type.reverse()
            X = calculateOverlap cuboid1.X cuboid2.X
            Y = calculateOverlap cuboid1.Y cuboid2.Y
            Z = calculateOverlap cuboid1.Z cuboid2.Z
        }
    else
        None

//Calculates the volume of a cuboid
let private calculateVolume (cuboid: Cuboid) =
    let (minX, maxX) = cuboid.X
    let (minY, maxY) = cuboid.Y
    let (minZ, maxZ) = cuboid.Z

    let volume =
        cuboid.Type.multiply() *
        bigint (abs (maxX - minX) + 1) *
        bigint (abs (maxY - minY) + 1) *
        bigint (abs (maxZ - minZ) + 1)

    volume

let parseInput (lines: string array) (restrain: bool) =
    lines
    |> Array.choose (fun line ->
        let splitLine = line.Split(" ")
        let cmd = splitLine[0]
        let cubeRange =
            digitRegex.Matches(splitLine[1])
            |> Seq.map (fun each -> int each.Value)
            |> Seq.toArray

        let minX = cubeRange.[0]
        let maxX = cubeRange.[1]
        let minY = cubeRange.[2]
        let maxY = cubeRange.[3]
        let minZ = cubeRange.[4]
        let maxZ = cubeRange.[5]

        if restrain && (maxX < -50 ||  minX > 50 || maxY < -50 || minY > 50 || maxZ < -50 ||  minZ > 50) then
            None
        else
            Some {
                Type = if cmd = "on" then Add else Subtract
                X = (minX, maxX)
                Y = (minY, maxY)
                Z = (minZ, maxZ)
            }
    )

let private solve (cuboids: Cuboid array) =
    ([], cuboids)
    ||> Array.fold (fun acc newCuboid ->
        acc
        |> List.append (acc |> List.choose (calculateOverlapBetween newCuboid))
        |> fun x -> if newCuboid.Type = Add then newCuboid :: x else x
    )
    |> List.sumBy calculateVolume

let part1 (input) =
    solve (parseInput input true)

let part2 (input) =
    solve (parseInput input false)