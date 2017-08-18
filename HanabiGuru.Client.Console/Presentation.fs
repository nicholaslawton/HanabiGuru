module HanabiGuru.Client.Console.Presentation

open System
open HanabiGuru.Engine

let private withConsoleColour colour task =
    let initial = Console.ForegroundColor
    Console.ForegroundColor <- colour
    task ()
    Console.ForegroundColor <- initial

let private task f x = fun () -> f x

let private taskWithConsoleColour colour f = task f >> withConsoleColour colour
let private cprintf colour format = taskWithConsoleColour colour (printf format)
let private cprint colour = taskWithConsoleColour colour printf

let private labelColour = ConsoleColor.DarkGray
let private structureColour = ConsoleColor.DarkGray
let private dataColour = ConsoleColor.White

let private printLabel = cprint labelColour
let private printStructure = cprint structureColour
let private printData = cprintf dataColour

let private lineBreakTask = task printfn ""

let private playersTasks players =
    let playerTask (Name name) = task (printData "%s") name
    task printLabel "Players"
    :: task printStructure ": "
    :: (players |> Set.toList |> List.map playerTask |> List.weave (task printStructure ", "))

let private playerViewTasks =
    let nameTask (name : string) =
        let nameWidth = 10
        name |> Seq.truncate nameWidth |> String.Concat |> task (printf "%*s" nameWidth)
    let cardTask (Card (suit, Rank rank)) =
        [
            suit |> sprintf "%A" |> Seq.take 1 |> String.Concat |> task (printf "%s")
            rank |> task (printf "%i")
        ]
        |> List.reduce (>>)
    let handTasks { player = Name name; cards = hand } =
        nameTask name
        :: task printf ": "
        :: List.weave (task printf " ") (List.map cardTask hand)

    PlayerView.otherHands
    >> function
        | [] -> PlayerHand.create (PlayerIdentity.create "") [] |> List.singleton
        | hands -> hands
    >> List.map handTasks
    >> List.map (List.reduce (>>))
    >> List.weave lineBreakTask

let game state =
    let tasks =
        match GameState.activePlayer state with
        | Some player -> GameState.playerView player state |> playerViewTasks
        | None -> GameState.players state |> playersTasks
    let composition = tasks |> List.reduce (>>) >> lineBreakTask
    composition ()

let commandFailure (failure : CannotPerformAction) = printfn "%A" failure

let invalidInput = printfn "%s"
