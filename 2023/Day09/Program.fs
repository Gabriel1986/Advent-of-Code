/// https://adventofcode.com/2023/day/9
module Year2023Day9
open System.Numerics
open System.Text.RegularExpressions

let private parseInput (input: string array) =
    let digitRegex = Regex(@"(-)?\d+")

    input
    |> Array.map (digitRegex.Matches >> Seq.map (fun x -> BigInteger.Parse x.Value) >> Seq.toList)

let private calculateColunn (stopCondition: bigint seq -> bigint seq -> bool) (subtract: bigint * bigint -> bigint) (currentLevelNumbers: bigint seq)  =
    let rec calculateLeftColumn (acc: bigint list) (currentLevelNumbers: bigint seq) =
        if stopCondition acc currentLevelNumbers then
            acc
        else
            currentLevelNumbers
            |> Seq.pairwise
            |> Seq.map subtract
            |> calculateLeftColumn ((currentLevelNumbers |> Seq.head)::acc)
    calculateLeftColumn [] currentLevelNumbers

let private findLastColumn (numbers: bigint list) =
    calculateColunn (fun _ acc -> acc |> Seq.head = BigInteger.Zero) (fun (a, b) -> a - b) (numbers |> Seq.rev)

let part1 (input) =
    parseInput input
    |> Array.sumBy (findLastColumn >> Seq.sum)

let private findFirstColumn (numbers: bigint list)  =
    let height = findLastColumn numbers |> Seq.length
    calculateColunn (fun acc _ -> acc |> Seq.length = height) (fun (a, b) -> b - a) numbers

let part2 (input) =
    parseInput input
    |> Array.sumBy (findFirstColumn >> List.fold (fun acc next -> next - acc) BigInteger.Zero)