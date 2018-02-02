module HanabiGuru.Client.Console.Presentation

open System
open HanabiGuru.Engine

let private withConsoleForeground colour task =
    let initial = Console.ForegroundColor
    Console.ForegroundColor <- colour
    task ()
    Console.ForegroundColor <- initial

let private withConsoleBackground colour task =
    let initial = Console.BackgroundColor
    Console.BackgroundColor <- colour
    task ()
    Console.BackgroundColor <- initial

let private task f x = fun () -> f x

let private taskWithConsoleForeground colour f = task f >> withConsoleForeground colour
let private taskWithConsoleBackground colour f = task f >> withConsoleBackground colour
let private taskWithConsoleColours background foreground =
    taskWithConsoleForeground foreground >> taskWithConsoleBackground background
let private cprint colour = taskWithConsoleForeground colour printf

let private labelColour = ConsoleColor.DarkGray
let private structureColour = ConsoleColor.DarkGray
let private dataColour = ConsoleColor.White
let private cardColour = function
    | Blue -> ConsoleColor.Cyan
    | Green -> ConsoleColor.DarkGreen
    | Red -> ConsoleColor.Red
    | Yellow -> ConsoleColor.Yellow
    | White -> ConsoleColor.White
let private cardBackground = ConsoleColor.DarkBlue
let private concealedCardBackground = ConsoleColor.DarkGray

let private printStaticLabel = cprint labelColour
let private printLabel = taskWithConsoleForeground labelColour
let private printStructure = cprint structureColour
let private printIntegerData = taskWithConsoleForeground dataColour <| printf "%2i"
let private printFloatData = taskWithConsoleForeground dataColour <| printf "%.1f"
let private printStringData = taskWithConsoleForeground dataColour <| printf "%s"

let private lineBreakTask = task printfn ""

let private playersTasks players =
    let playerTask (Name name) = name |> task printStringData
    task printStaticLabel "Players"
    :: task printStructure ": "
    :: (players |> List.map playerTask |> List.weave (task printStructure ", "))

let private cardTask backgroundColour (Card (suit, Rank rank)) =
    task (taskWithConsoleColours backgroundColour (cardColour suit) (printf "%i")) rank

let private otherHandsTasks view =
    let nameTask (name : string) =
        let nameWidth = 10
        let printName = printLabel <| printf "%*s" nameWidth
        name |> Seq.truncate nameWidth |> String.Concat |> task printName
    let handTasks { player = Name name; cards = hand } =
        nameTask name
        :: task printStructure ": "
        :: List.weave
            (task printStructure " ")
            (List.map (fun { identity = card } -> cardTask cardBackground card) hand)

    view
    |> PlayerView.otherPlayers
    |> List.map (fun otherPlayer -> PlayerView.otherHand otherPlayer view)
    |> List.map handTasks
    |> List.map (List.reduce (>>))
    |> List.weave lineBreakTask

let private candidateIdentityTask { card = card; probability = p } =
    let probabilityTask = 
        [
            task printStructure "("
            task printFloatData (p * 100.0)
            task printStructure "%%)"
        ]
        |> List.reduce (>>)
    [cardTask concealedCardBackground card; probabilityTask]
    |> List.weave (task printStructure " ")
    |> List.reduce (>>)

let private ownCardTask candidateIdentities =
    candidateIdentities
    |> List.map candidateIdentityTask
    |> List.truncate 5
    |> List.weave (task printStructure " ")
    |> List.reduce (>>)

let private ownCardsTasks view =
    PlayerView.hand view
    |> List.map (PlayerView.CardIdentity.deduce view)
    |> List.map ownCardTask
    |> List.weave lineBreakTask

let private playerViewTasks view =
    let numericStateTask label value =
        [
            task printStaticLabel label
            task printStructure ": "
            task printIntegerData value
        ]
        |> List.reduce (>>)
    let stateTasks =
        [
            numericStateTask "Draw deck" (PlayerView.drawDeckSize view)
            numericStateTask "Clock tokens" (PlayerView.clockTokens view)
            numericStateTask "Fuse tokens" (PlayerView.fuseTokens view)
        ]
        |> List.weave (task printStructure "    ")
    stateTasks @ lineBreakTask :: otherHandsTasks view @ lineBreakTask :: ownCardsTasks view

let game state =
    let tasks =
        match GameState.activePlayer state with
        | Some player -> GameState.playerView player state |> playerViewTasks
        | None -> GameState.players state |> playersTasks
    let composition = tasks |> List.reduce (>>) >> lineBreakTask
    composition ()

let commandFailure failure =
    let message summary = function
        | [] -> sprintf "%s: reason unspecified" summary
        | [reason] -> sprintf "%s: %s" summary reason
        | reasons ->
            reasons
            |> List.map (sprintf " - %s")
            |> List.reduce (sprintf "%s\n%s")
            |> sprintf "%s for the following reasons:\n%s" summary
    let display = function
        | InvalidCommand reasons -> ("Invalid command", reasons |> List.map (function
            | InvalidCardIdentifier -> "invalid card selection"))
        | ExecutionFailure executionFailure ->
            match executionFailure with
            | CannotAddPlayer reasons ->
                ("Cannot add player", reasons |> List.map (function
                    | PlayerAlreadyJoined -> "that player has already joined the game"
                    | NoSeatAvailable -> "there are no more seats available"
                    | CannotAddPlayerReason.GameAlreadyStarted -> "the game has already started"))
            | CannotStartGame reasons ->
                ("Cannot start game", reasons |> List.map (function
                    | WaitingForMinimumPlayers ->
                        sprintf "waiting for the minimum number of players (%i)" GameRules.minimumPlayers
                    | GameAlreadyStarted -> "the game has already started"))
            | CannotTakeTurn reasons ->
                ("Cannot take turn", reasons |> List.map (function
                    | GameNotStarted -> "the game has not started"
                    | GameOver -> "the game is over"))
            | CannotGiveInformation reasons ->
                ("Cannot give information", reasons |> List.map (function
                    | NoClockTokensAvailable -> "no clock tokens are available"
                    | NoMatchingCards -> "at least one card must match the information given"
                    | InvalidRecipient -> "the recipient must be one of the other players in the game"))
            | CannotDiscardCard reasons ->
                ("Cannot discard card", reasons |> List.map (function
                    | AllClockTokensAvailable -> "all clock tokens are available"
                    | CardNotInHand -> "this card is not in your hand"))
    let summary, reasons = (display failure)
    message summary reasons |> printfn "%s"

let invalidInput = printfn "%s"
