module HanabiGuru.Client.Console.Tests.CommandProcessingTestFramework

open Swensen.Unquote
open HanabiGuru.Client.Console
open HanabiGuru.Engine

let processInput input =
    let mutable inputQueue = input

    let getInput () =
        match inputQueue with
        | input :: remainingInput ->
            inputQueue <- remainingInput
            Some input
        | [] -> None

    let mutable gameState = EventHistory.empty
    let commandExecuted newGameState = gameState <- newGameState
    let fail failure = new AssertionFailedException(sprintf "%A" failure) |> raise

    CommandInterface.processInput getInput (CommandInterface.pipeline commandExecuted fail fail)

    gameState

let startGame names =
    let addPlayers = names |> Set.toList |> List.map (sprintf "add player %s")
    "start" :: addPlayers |> List.rev |> processInput

let giveInformation game =
    let recipient = GameState.players game |> List.find (Some >> (<>) (GameState.activePlayer game))
    let cardTrait =
        GameState.hands game
        |> List.filter (fun hand -> hand.player = recipient)
        |> List.collect (fun hand -> hand.cards)
        |> List.map (fun { identity = Card (suit, _) } -> SuitTrait suit)
        |> List.head
    Game.giveInformation recipient cardTrait game

let private firstCardOfActiveHand game =
    GameState.playerView (GameState.activePlayer game |> Option.get) game
    |> PlayerView.hand
    |> List.head

let discardCard game =
    Game.discard (firstCardOfActiveHand game) game

let playCard game =
    Game.playCard (firstCardOfActiveHand game) game
