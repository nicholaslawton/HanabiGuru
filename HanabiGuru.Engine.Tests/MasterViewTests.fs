module HanabiGuru.Engine.Tests.MasterViewTests

open FsCheck
open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property>]
let ``After adding a player, the view contains one more player`` (view : MasterView) (player : PlayerIdentity) =
    MasterView.addPlayer view player
    |> fun v -> v.players
    |> List.length =! List.length view.players + 1

[<Property>]
let ``After adding a player, the view contains the added player`` (view : MasterView) (player : PlayerIdentity) =
    MasterView.addPlayer view player
    |> fun v -> v.players
    |> List.filter (fun p -> p.identity = player)
    |> List.length >! 0

[<Property>]
let ``After adding a player, the players in the view are sorted in turn order``
    (view : MasterView)
    (player : PlayerIdentity) =

    MasterView.addPlayer view player |> fun v -> v.players =! List.sort v.players

[<Property>]
let ``Adding fuse tokens increases the number of fuse tokens in the view`` (view : MasterView) (PositiveInt count) =
    [1..count]
    |> List.fold (fun v _ -> MasterView.addFuseToken v) view
    |> fun v -> v.fuseTokens =! view.fuseTokens + count

[<Property>]
let ``Adding clock tokens increases the number of clock tokens in the view`` (view : MasterView) (PositiveInt count) =
    [1..count]
    |> List.fold (fun v _ -> MasterView.addClockToken v) view
    |> fun v -> v.clockTokens =! view.clockTokens + count

[<Property>]
let ``After adding a card to the draw deck, the view contains one more card`` (view : MasterView) (card : Card) =
    MasterView.addCardToDrawDeck view card
    |> fun v -> v.drawDeck
    |> List.length =! List.length view.drawDeck + 1

[<Property>]
let ``After adding a card to the draw deck, the view contains the added card`` (view : MasterView) (card : Card) =
    MasterView.addCardToDrawDeck view card
    |> fun v -> v.drawDeck
    |> List.filter ((=) card)
    |> List.length >! 0

let private dealCardToPlayerTest view card player property =
    let viewWithCardAndPlayer =
        { view with
            players = PlayerState.create player :: view.players |> List.distinctBy (fun player -> player.identity)
            drawDeck = if List.contains card view.drawDeck then view.drawDeck else card :: view.drawDeck
        }
    let newView = MasterView.dealCardToPlayer viewWithCardAndPlayer card player

    property newView viewWithCardAndPlayer

[<Property>]
let ``Dealing a card to a player does not change the total set of cards in the game``
    (view : MasterView)
    (card : Card)
    (player : PlayerIdentity) =

    let cards view =
        List.collect id
            [
                view.players |> List.collect (fun player -> player.hand)
                view.drawDeck
                view.fireworks
                view.discard
            ]

    let property newView view = test <@ List.sort (cards newView) = List.sort (cards view) @>

    dealCardToPlayerTest view card player property

[<Property>]
let ``Dealing a card to a player increases the number of cards in the player's hand``
    (view : MasterView)
    (card : Card)
    (player : PlayerIdentity) =

    let handLength playerIdentity view =
        view.players
        |> List.filter (fun player -> player.identity = playerIdentity)
        |> List.map (fun player -> List.length player.hand)
        |> List.head

    let property newView view = test <@ handLength player newView = handLength player view + 1 @>

    dealCardToPlayerTest view card player property

[<Property>]
let ``After dealing a card to a player, the player's hand contains the card dealt``
    (view : MasterView)
    (card : Card)
    (player : PlayerIdentity) =

    let property newView _ =
        test <@ newView.players
        |> List.filter (fun p -> p.identity = player)
        |> List.map (fun p -> p.hand |> List.filter ((=) card))
        |> List.isEmpty
        |> not @>

    dealCardToPlayerTest view card player property

[<Property>]
let ``Dealing a card which does not exist in the draw deck has no affect``
    (view : MasterView)
    (card : Card)
    (player : PlayerIdentity) =

    let viewWithPlayerAndWithoutCard =
        { view with
            players = PlayerState.create player :: view.players
            drawDeck = List.filter ((<>) card) view.drawDeck
        }
    
    let newView = MasterView.dealCardToPlayer viewWithPlayerAndWithoutCard card player

    newView =! viewWithPlayerAndWithoutCard

[<Property>]
let ``Dealing to a player who is not in the game has no affect``
    (view : MasterView)
    (card : Card)
    (player : PlayerIdentity) =

    let viewWithoutPlayerAndWithCard =
        { view with
            players = List.filter (fun p -> p.identity <> player) view.players
            drawDeck = card :: view.drawDeck
        }
    
    let newView = MasterView.dealCardToPlayer viewWithoutPlayerAndWithCard card player

    newView =! viewWithoutPlayerAndWithCard
