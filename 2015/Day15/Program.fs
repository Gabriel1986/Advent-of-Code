/// https://adventofcode.com/2015/day/15
module Year2015Day15

let numberRegex = Regex.numberRegex
let parse (input: string) =
    let matches = numberRegex.Matches(input)
    [| int matches[0].Value; int matches[1].Value; int matches[2].Value; int matches[3].Value; int matches[4].Value |]


let rec generateCombinations (maxAmountToUse: int) (nbOfIngredients: int): seq<int[]> =
    seq {
        if nbOfIngredients = 1 then
            yield [| maxAmountToUse |]
        else
            for i in 0..maxAmountToUse do
                for rest in generateCombinations (maxAmountToUse - i) (nbOfIngredients - 1) do
                    let arr = Array.zeroCreate nbOfIngredients
                    arr[0] <- i
                    Array.blit rest 0 arr 1 (nbOfIngredients - 1)
                    yield arr
    }

let calculate (guardCalories: bool) (ingredients: (int * int[])[]) (amountOfIngredients: int array) =
    let mutable score = 1
    for propertyIndex in 0..(if guardCalories then 4 else 3) do
        let mutable totalForProperty = 0
        for i = 0 to ingredients.Length - 1 do
            let ingredientIdx, ingredient = ingredients[i]
            totalForProperty <- totalForProperty + (ingredient[propertyIndex] * amountOfIngredients[ingredientIdx])
        if propertyIndex = 4 && guardCalories then
            if totalForProperty <> 500 then score <- 0
        else
            score <- score * max 0 totalForProperty
    score

let part1 (input: string[]) =
    let maxAmountToUse = 100
    let parsed =
        input
        |> Array.map parse
        |> Array.indexed

    generateCombinations maxAmountToUse parsed.Length
    |> Seq.chunkBySize 10000
    |> Seq.map (fun batch ->
        batch
        |> Array.Parallel.map (calculate false parsed)
        |> Array.max)
    |> Seq.max

let part2 (input: string[]) =
    let maxAmountToUse = 100
    let parsed =
        input
        |> Array.map parse
        |> Array.indexed

    generateCombinations maxAmountToUse parsed.Length
    |> Seq.chunkBySize 10000
    |> Seq.map (fun batch ->
        batch
        |> Array.Parallel.map (calculate true parsed)
        |> Array.max)
    |> Seq.max