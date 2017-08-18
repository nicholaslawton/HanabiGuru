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

let private printStaticLabel = cprint labelColour
let private printLabel = taskWithConsoleForeground labelColour
let private printStructure = cprint structureColour
let private printData = taskWithConsoleForeground dataColour

let private lineBreakTask = task printfn ""

let private playersTasks players =
    let printName = printData <| printf "%s"
    let playerTask (Name name) = name |> task printName
    task printStaticLabel "Players"
    :: task printStructure ": "
    :: (players |> Set.toList |> List.map playerTask |> List.weave (task printStructure ", "))

let private cardTask (Card (suit, Rank rank)) =
    task (taskWithConsoleColours cardBackground (cardColour suit) (printf "%i")) rank

let private playerViewTasks =
    let nameTask (name : string) =
        let nameWidth = 10
        let printName = printLabel <| printf "%*s" nameWidth
        name |> Seq.truncate nameWidth |> String.Concat |> task printName
    let handTasks { player = Name name; cards = hand } =
        nameTask name
        :: task printStructure ": "
        :: List.weave (task printStructure " ") (List.map cardTask hand)

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
