module HanabiGuru.Client.Console.Presentation

open System
open HanabiGuru.Engine

let private task f x = fun () -> f x

let lineBreakTask = task printfn ""

let private playersTasks players =
    let playerTask (Name name) = task (printf "%s") name
    task printf "Players: "
    :: (Set.toList players |> List.map playerTask |> List.weave (task printf ", "))

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
