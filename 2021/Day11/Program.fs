/// https://adventofcode.com/2021/day/11
module Year2021Day11

let private parseInput (input: string array) =
    Array2D.init (input.Length) (input[0].Length) (fun i j -> int (string (input[i][j])))

let part1 (input: string array) =
    let inputArray = parseInput (input)

    let calculateNbFlashes (nbIterations: int) =
        let rec increaseNumber (i: int, j: int) =
            if (i < 0 || i = inputArray.GetLength(0) || j < 0 || j = inputArray.GetLength(1)) then
                ()
            else
                match inputArray[i, j] with
                | 9 ->
                    inputArray[i, j] <- 10
                    increaseNumber (i-1, j)
                    increaseNumber (i, j-1)
                    increaseNumber (i+1, j)
                    increaseNumber (i, j+1)
                    increaseNumber (i-1, j-1)
                    increaseNumber (i+1, j+1)
                    increaseNumber (i+1, j-1)
                    increaseNumber (i-1, j+1)
                | 10 ->
                    ()
                | other ->
                    inputArray[i, j] <- other + 1
                    ()

        let countFlashesAndReset () =
            let mutable counter = 0
            inputArray
            |> Array2D.iteri (fun i j value ->
                if value = 10 then
                    counter <- counter + 1
                    inputArray.SetValue(0, i, j)
                else
                    ()
            )
            counter

        let rec calculateNbFlashes (nbFlashes: int, currentIteration: int) =
            if currentIteration = nbIterations then nbFlashes
            else
                for i in 0..inputArray.GetUpperBound(0) do
                    for j in 0..inputArray.GetUpperBound(1) do
                        increaseNumber (i, j)
                calculateNbFlashes (nbFlashes + countFlashesAndReset (), currentIteration + 1)

        calculateNbFlashes (0, 0)

    calculateNbFlashes 100

let part2 (input: string array) =
    let inputArray = parseInput (input)
    let nbNumbersInInput = inputArray.Length

    let calculateSynchronizationPoint () =
        let rec increaseNumber (i: int, j: int) =
            if (i < 0 || i = inputArray.GetLength(0) || j < 0 || j = inputArray.GetLength(1)) then
                ()
            else
                match inputArray[i, j] with
                | 10 ->
                    ()
                | 9 ->
                    inputArray.SetValue(10, i, j)
                    increaseNumber (i-1, j)
                    increaseNumber (i, j-1)
                    increaseNumber (i+1, j)
                    increaseNumber (i, j+1)
                    increaseNumber (i-1, j-1)
                    increaseNumber (i+1, j+1)
                    increaseNumber (i+1, j-1)
                    increaseNumber (i-1, j+1)
                | other ->
                    inputArray.SetValue(other + 1, i, j)
                    ()

        //Using mutable here to not have to go over the entire array twice...
        let countFlashesAndReset () =
            let mutable counter = 0
            inputArray
            |> Array2D.iteri (fun i j value ->
                if value = 10 then
                    counter <- counter + 1
                    inputArray.SetValue(0, i, j)
                else
                    ()
            )
            counter

        let rec calculateSynchronizationPoint (nbFlashes: int, currentIteration: int) =
            if nbFlashes = nbNumbersInInput then currentIteration
            else
                inputArray |> Array2D.iteri (fun i j _ -> increaseNumber (i, j))
                calculateSynchronizationPoint (countFlashesAndReset (), currentIteration + 1)

        calculateSynchronizationPoint (0, 0)

    calculateSynchronizationPoint ()