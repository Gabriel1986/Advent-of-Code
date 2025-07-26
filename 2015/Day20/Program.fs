/// https://adventofcode.com/2015/day/20
module Year2015Day20

let findLowestHousePart1 (targetPresents: int) =
    let maxHouse = targetPresents / 10
    let presents = Array.zeroCreate (maxHouse + 1)

    for elf = 1 to maxHouse do
        let mutable house = elf
        while house <= maxHouse do
            presents[house] <- presents[house] + elf * 10
            house <- house + elf

    presents
    |> Array.findIndex (fun sum -> sum >= targetPresents)

let findLowestHousePart2 (targetPresents: int) =
    let maxHouse = targetPresents / 11
    let presents = Array.zeroCreate (maxHouse + 1)

    for elf = 1 to maxHouse do
        let mutable visitCount = 0
        let mutable house = elf
        while house <= maxHouse && visitCount < 50 do
            presents[house] <- presents[house] + (11 * elf)
            house <- house + elf
            visitCount <- visitCount + 1

    presents
    |> Array.findIndex (fun p -> p >= targetPresents)

let part1 (input: string[]) =
    findLowestHousePart1 (input[0] |> int)

let part2 (input: string[]) =
    findLowestHousePart2 (input[0] |> int)