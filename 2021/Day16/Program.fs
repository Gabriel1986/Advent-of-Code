/// https://adventofcode.com/2021/day/16
module Year2021Day16
open System

type Packet = {
    TypeId: int
    Type: PacketType
    Version: int
    Length: int
}
and PacketType =
    | Literal of value: Int64
    | Operator of subPackets: Packet list

let private parseInput (input: string array) =
    input[0]
    |> Seq.map convertToBinary
    |> String.joinWith ""

let private readLiteralChunks =
    Seq.chunkBySize 5
    >> (fun chunks -> chunks |> Seq.take (chunks |> Seq.findIndex (fun chunk -> chunk[0] = '0') |> (+) 1))
    >> List.ofSeq

let rec private readPackets (input: string, alreadyReadPackets: Packet list, nbPacketsToRead: int option): Packet list =
    if input.Trim('0').Length = 0 || nbPacketsToRead = Some 0 then
        alreadyReadPackets
    else
        let packetVersion = Convert.ToInt32(input[0..2], 2)
        let packetTypeId = Convert.ToInt32(input[3..5], 2)

        let createLiteralPacket (baseLength: int, literalChunks: char[] list) = {
            TypeId = packetTypeId
            Version = packetVersion
            Length = baseLength + (literalChunks |> List.sumBy (fun each -> each.Length))
            Type = Literal (literalChunks |> List.map (Array.tail >> String) |> String.joinWith "" |> (fun x -> Convert.ToInt64(x, 2)))
        }

        let createOperatorPacket (baseLength: int, subPackets: Packet list) = {
            TypeId = packetTypeId
            Version = packetVersion
            Length = baseLength + (subPackets |> List.sumBy (fun each -> each.Length))
            Type = Operator subPackets
        }

        let packet =
            if packetTypeId = 4 then
                let chunks = readLiteralChunks (input[6..])
                createLiteralPacket (6, chunks)
            elif input[6] = '0' then
                let length = Convert.ToInt32 (input[7..21], 2)
                let subPackets = readPackets (input[22..22+length-1], [], None)
                createOperatorPacket (22, subPackets)
            else
                let nbPackets = Convert.ToInt32 (input[7..17], 2)
                let subPackets = readPackets (input[18..], [], Some nbPackets)
                createOperatorPacket (18, subPackets)

        readPackets(input[packet.Length..], alreadyReadPackets@[ packet ], nbPacketsToRead |> Option.map (fun x -> x - 1))

let part1 (input) =
    let rootPacket = readPackets (parseInput (input), [], None) |> List.head

    let rec sumOfVersions (packet: Packet) =
        match packet.Type with
        | Literal _ ->
            packet.Version
        | Operator o ->
            packet.Version + (o |> Seq.sumBy sumOfVersions)

    sumOfVersions rootPacket

let part2 (input) =
    let rootPacket = readPackets (parseInput (input), [], None) |> List.head

    let rec calculatePacket (packet: Packet) =
        match packet.Type with
        | Literal value -> value
        | Operator subPackets ->
            let calculatedSubPackets = subPackets |> List.map calculatePacket
            match packet.TypeId with
            | 0 -> calculatedSubPackets |> List.fold (+) 0
            | 1 -> calculatedSubPackets |> List.fold (*) 1
            | 2 -> calculatedSubPackets |> List.min
            | 3 -> calculatedSubPackets |> List.max
            | 5 -> if calculatedSubPackets[0] > calculatedSubPackets[1] then 1 else 0
            | 6 -> if calculatedSubPackets[0] < calculatedSubPackets[1] then 1 else 0
            | 7 -> if calculatedSubPackets[0] = calculatedSubPackets[1] then 1 else 0
            | other -> failwithf "Unexpected typeId: %i" other

    bigint (calculatePacket rootPacket)