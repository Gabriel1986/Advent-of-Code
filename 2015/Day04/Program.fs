/// https://adventofcode.com/2015/day/4
module Year2015Day4

open System
open System.Text
open System.Security.Cryptography
open System.Threading

type WorkerMessage =
    | Found of int
    | Done

let parallelBruteForce (prefix: string) (isValid: byte[] -> bool) =
    let result = ref -1
    use tokenSource = new CancellationTokenSource()
    let token = tokenSource.Token

    let processorCount = Environment.ProcessorCount
    let threads =
        [|
            for i in 0 .. processorCount - 1 ->
                Thread(fun () ->
                    use md5 = MD5.Create()
                    let rec computeNextHash (i: int) =
                        if token.IsCancellationRequested then
                            ()
                        else
                            let input = prefix + string i
                            let bytes = Encoding.ASCII.GetBytes input
                            let hash = md5.ComputeHash bytes
                            if isValid hash then
                                if Interlocked.CompareExchange(result, i, -1) = -1 then
                                    tokenSource.Cancel()
                            computeNextHash(i + processorCount)
                    computeNextHash i
                )
        |]

    threads |> Array.iter (fun t -> t.Start())
    threads |> Array.iter (fun t -> t.Join())
    result.Value

let part1 (input: string[]): int =
    parallelBruteForce input[0] (fun hash ->
        hash[0] = 0x00uy && hash[1] = 0x00uy && hash[2] < 0x10uy
    )

let part2 (input: string[]): int =
    parallelBruteForce input[0] (fun hash ->
        hash[0] = 0x00uy && hash[1] = 0x00uy && hash[2] = 0x00uy
    )