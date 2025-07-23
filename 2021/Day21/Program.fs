/// https://adventofcode.com/2021/day/21
module Year2021Day21
open System.Text.RegularExpressions

type Player = { Position: int; Score: int }

type SimpleGane = {
    DieFace: int
    DiceRolls: int
    Player1: Player
    Player2: Player
}

let lastDigitRegex = Regex(@"\d+", RegexOptions.RightToLeft ||| RegexOptions.Compiled)
//Working with positions 0 -> 9 in stead of 1 -> 10 to make it easier to just do modulo 10
let parseInput (input: string array) =
    let givenPositionPlayer1 = (lastDigitRegex.Match input[0]).Value  |> int
    let givenPositionPlayer2 = (lastDigitRegex.Match input[1]).Value  |> int
    givenPositionPlayer1 - 1, givenPositionPlayer2 - 1

let part1 (input: string[]) =
    let (givenPositionPlayer1, givenPositionPlayer2) = parseInput input

    let mutable game = {
        DieFace = 0
        DiceRolls = 0
        Player1 = { Position = givenPositionPlayer1; Score = 0 }
        Player2 = { Position = givenPositionPlayer2; Score = 0 }
    }

    while game.Player1.Score < 1000 && game.Player2.Score < 1000 do
        let nextSum = (game.DieFace * 3 + 6) % 100
        let nextDieFace = (game.DieFace + 3) % 100
        let player1Playing = game.DiceRolls % 2 = 0
        let player = if player1Playing then game.Player1 else game.Player2
        let updatedPosition = (player.Position + nextSum) % 10
        let updatedScore = player.Score + updatedPosition + 1
        let updatedPlayer = { Position = updatedPosition; Score = updatedScore }
        game <- {
            DieFace = nextDieFace
            DiceRolls = game.DiceRolls + 3
            Player1 = if player1Playing then updatedPlayer else game.Player1
            Player2 = if player1Playing then game.Player2 else updatedPlayer
        }

    let losingScore = if game.Player1.Score >= 1000 then game.Player2.Score else game.Player1.Score
    game.DiceRolls * losingScore

type GameState =
    {
        Player1Playing: bool
        Count: bigint
        Player1: Player
        Player2: Player
    }
    member this.Player = if this.Player1Playing then this.Player1 else this.Player2

module GameState =
    let initial givenPositionPlayer1 givenPositionPlayer2 = {
        Player1Playing = true
        Count = bigint 1
        Player1 = { Position = givenPositionPlayer1; Score = 0 }
        Player2 = { Position = givenPositionPlayer2; Score = 0 }
    }

let part2 (input: string[]) =
    let (givenPositionPlayer1, givenPositionPlayer2) = parseInput input

    //Compresses the list of game states by combining those with the same (player playing + player positions)
    let compress (games: GameState list) =
        games
        |> List.groupBy (fun u -> u.Player1Playing, u.Player1, u.Player2)
        |> List.map (fun (((player1Playing, player1, player2)), group) ->
            { Player1Playing = player1Playing; Player1 = player1; Player2 = player2; Count = group |> List.sumBy (fun grp -> grp.Count) })

    //For combination of rolls x + y + z, how many times does it happen?
    let possibleOutcomes = [ (3, bigint 1); (4, bigint 3); (5, bigint 6); (6, bigint 7); (7, bigint 6); (8, bigint 3); (9, bigint 1) ]

    //Combines the current game state with all possible outcomes
    let nextGameStates (game: GameState): GameState list =
        possibleOutcomes
        |> List.map (fun (sumOfDice, numberOfPossibilities) ->
            let updatedPosition = (game.Player.Position + sumOfDice) % 10
            let updatedPlayer = { Position = updatedPosition; Score = game.Player.Score + updatedPosition + 1 }
            {
                Player1Playing = not game.Player1Playing
                Count = game.Count * numberOfPossibilities
                Player1 = if game.Player1Playing then updatedPlayer else game.Player1
                Player2 = if game.Player1Playing then game.Player2 else updatedPlayer
            })

    //Could have done this with recursion, but this allocates less stack
    let mutable gamesInProgress = [GameState.initial givenPositionPlayer1 givenPositionPlayer2]
    let mutable gamesWon = (bigint 0, bigint 0)

    while gamesInProgress.Length > 0 do
        let inProgress, finished =
            gamesInProgress
            |> List.collect nextGameStates
            |> List.partition (fun u -> u.Player1.Score < 21 && u.Player2.Score < 21)

        let wonByPlayer1, wonByPlayer2 = finished |> List.partition _.Player1Playing
        gamesWon <- (fst gamesWon + (wonByPlayer1 |> List.sumBy _.Count), snd gamesWon + (wonByPlayer2 |> List.sumBy _.Count))
        gamesInProgress <- compress inProgress

    gamesWon ||> max