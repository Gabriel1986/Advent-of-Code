/// https://adventofcode.com/2021/day/18
module Year2021Day18

open System

type NodeType =
    | Value of int
    | Node of Node

and Node =
    { Left: NodeType
      Right: NodeType }

/// Result type for explosion
type ExplodeResult = {
    DidExplode: bool
    LeftCarry: int option
    RightCarry: int option
    NewNode: NodeType
}

/// Result type for splitting
type SplitResult = {
    DidSplit: bool
    NewNode: NodeType
}

/// Immutable add to leftmost value in a tree
let rec addToLeftMost (node: NodeType) (value: int) : NodeType =
    match node with
    | Value v -> Value (v + value)
    | Node n -> Node { n with Left = addToLeftMost n.Left value }

/// Immutable add to rightmost value in a tree
let rec addToRightMost (node: NodeType) (value: int) : NodeType =
    match node with
    | Value v -> Value (v + value)
    | Node n -> Node { n with Right = addToRightMost n.Right value }

/// Try to explode a node at given depth
let rec tryExplode (node: NodeType) (depth: int) : ExplodeResult =
    match node with
    | Value _ -> 
        { DidExplode = false; LeftCarry = None; RightCarry = None; NewNode = node }
    | Node n when depth >= 4 ->
        match n.Left, n.Right with
        | Value l, Value r -> 
            { DidExplode = true; LeftCarry = Some l; RightCarry = Some r; NewNode = Value 0 }
        | _ -> 
            { DidExplode = false; LeftCarry = None; RightCarry = None; NewNode = node }
    | Node n ->
        let leftResult: ExplodeResult = tryExplode n.Left (depth + 1)
        if leftResult.DidExplode then
            let newRight: NodeType =
                match leftResult.RightCarry with
                | Some v -> addToLeftMost n.Right v
                | None -> n.Right
            { DidExplode = true
              LeftCarry = leftResult.LeftCarry
              RightCarry = None
              NewNode = Node { Left = leftResult.NewNode; Right = newRight } }
        else
            let rightResult: ExplodeResult = tryExplode n.Right (depth + 1)
            if rightResult.DidExplode then
                let newLeft: NodeType =
                    match rightResult.LeftCarry with
                    | Some v -> addToRightMost n.Left v
                    | None -> n.Left
                { DidExplode = true
                  LeftCarry = None
                  RightCarry = rightResult.RightCarry
                  NewNode = Node { Left = newLeft; Right = rightResult.NewNode } }
            else
                { DidExplode = false; LeftCarry = None; RightCarry = None; NewNode = node }

/// Try to split a node
let rec trySplit (node: NodeType) : SplitResult =
    match node with
    | Value v when v >= 10 ->
        let leftVal: int = v / 2
        let rightVal: int = (v + 1) / 2
        { DidSplit = true
          NewNode = Node { Left = Value leftVal; Right = Value rightVal } }
    | Value _ ->
        { DidSplit = false; NewNode = node }
    | Node n ->
        let leftResult: SplitResult = trySplit n.Left
        if leftResult.DidSplit then
            { DidSplit = true
              NewNode = Node { n with Left = leftResult.NewNode } }
        else
            let rightResult: SplitResult = trySplit n.Right
            if rightResult.DidSplit then
                { DidSplit = true
                  NewNode = Node { n with Right = rightResult.NewNode } }
            else
                { DidSplit = false; NewNode = node }

/// Reduce a snailfish number by repeatedly exploding and splitting
let rec reduce (node: NodeType) : NodeType =
    let explodeResult: ExplodeResult = tryExplode node 0
    if explodeResult.DidExplode then
        reduce explodeResult.NewNode
    else
        let splitResult: SplitResult = trySplit node
        if splitResult.DidSplit then
            reduce splitResult.NewNode
        else
            node

/// Add two snailfish numbers and reduce
let add (left: NodeType) (right: NodeType) : NodeType =
    reduce (Node { Left = left; Right = right })

/// Compute magnitude of a snailfish number
let rec magnitude (node: NodeType) : int =
    match node with
    | Value v -> v
    | Node n -> 3 * magnitude n.Left + 2 * magnitude n.Right

/// Parse a snailfish number from string into a NodeType
let parse (input: string) : NodeType =
    let chars: char[] = input.ToCharArray()

    let rec parseInternal (index: int) : NodeType * int =
        match chars.[index] with
        | '[' ->
            let left, afterLeft = parseInternal (index + 1)
            let afterComma = afterLeft + 1 // skip ','
            let right, afterRight = parseInternal afterComma
            let afterBracket = afterRight + 1 // skip ']'
            Node { Left = left; Right = right }, afterBracket
        | c when System.Char.IsDigit c -> Value (int (string c)), index + 1
        | other -> failwithf "Unexpected character %c at position %d" other index

    let tree, _ = parseInternal 0
    tree

/// Part 1 and 2 using immutable trees

let part1 (input: string array) =
    input
    |> Array.map parse
    |> Array.reduce add
    |> magnitude

let part2 (input: string array) =
    let snailfishNumbers = input |> Array.map parse
    
    snailfishNumbers
    |> Array.Parallel.mapi (fun i left ->
        snailfishNumbers
        |> Array.Parallel.mapi (fun j right ->
            if i <> j then magnitude (add left right) else Int32.MinValue
        )
        |> Array.max
    )
    |> Array.max