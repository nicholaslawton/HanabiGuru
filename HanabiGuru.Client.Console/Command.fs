namespace HanabiGuru.Client.Console

open HanabiGuru.Engine

type Command =
    | AddPlayer of string
    | StartGame
    | GiveInformation of string * CardTrait
    | DiscardCard of char

type InvalidCommandReason =
    | InvalidCardIdentifier

type CommandError =
    | InvalidCommand of InvalidCommandReason list
    | ExecutionFailure of CannotPerformAction

module Command =

    let selectCard cardId cards =
        ['a'..'z']
        |> List.replicate (List.length cards + 1)
        |> List.concat
        |> List.truncate (List.length cards)
        |> List.zip cards
        |> List.filter (snd >> (=) cardId)
        |> List.map fst
        |> List.tryHead
        |> Option.map Ok
        |> Option.defaultValue (Error (InvalidCommand [InvalidCardIdentifier]))

    let execute game =
        let getActivePlayerHand game =
            GameState.activePlayer game
            |> Option.map (fun activePlayer -> GameState.playerView activePlayer game |> PlayerView.hand |> Ok)
            |> Option.defaultValue (Error (ExecutionFailure (CannotTakeTurn [GameNotStarted])))

        function
        | AddPlayer name ->
            Game.addPlayer (PlayerIdentity.create name) game |> Result.mapError ExecutionFailure
        | StartGame ->
            Game.startGame game |> Result.mapError ExecutionFailure
        | GiveInformation (name, cardTrait) ->
            Game.giveInformation (PlayerIdentity.create name) cardTrait game |> Result.mapError ExecutionFailure
        | DiscardCard cardId ->
            getActivePlayerHand game
            |> Result.bind (selectCard cardId)
            |> Result.bind (fun card -> Game.discard card game |> Result.mapError ExecutionFailure)
