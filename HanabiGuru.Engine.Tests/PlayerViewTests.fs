module HanabiGuru.Engine.Tests.PlayerViewTests

open FsCheck.Xunit
open Swensen.Unquote
open HanabiGuru.Engine

[<Property>]
let ``Sorting other players in relative turn order does not change the number of players`` (view : PlayerView) =
    let newView = PlayerView.sortOtherPlayersInRelativeTurnOrder view
    List.length newView.otherPlayers =! List.length view.otherPlayers

[<Property>]
let ``When other players are sorted in relative turn order, there is no more than one decrease in sequence``
    (view : PlayerView) =

    (PlayerView.sortOtherPlayersInRelativeTurnOrder view).otherPlayers
    |> List.pairwise
    |> List.filter (fun (x, y) -> x > y)
    |> List.length <! 2

[<Property>]
let ``Sorting other players in relative turn order places players following self at the front of the list in order``
    (view : PlayerView) =

    let newView = PlayerView.sortOtherPlayersInRelativeTurnOrder view
    let followingPlayers =
        newView.otherPlayers
        |> List.takeWhile ((<) view.self)
    let expectedFollowingPlayers =
        view.otherPlayers
        |> List.filter ((<) view.self)
        |> List.sort
    followingPlayers =! expectedFollowingPlayers

[<Property(Arbitrary = [| typeof<DistinctPlayers> |])>] 
let ``Sorting other players in relative turn order places players preceding self at the back of the list in order``
    (ValidPlayerView view) =

    let newView = PlayerView.sortOtherPlayersInRelativeTurnOrder view
    let precedingPlayers =
        newView.otherPlayers
        |> List.skipWhile ((<) view.self)
    let expectedPrecedingPlayers =
        view.otherPlayers
        |> List.filter ((>) view.self)
        |> List.sort
    precedingPlayers =! expectedPrecedingPlayers

[<Property>]
let ``After adding a player, the view contains one more player`` (view : PlayerView) (player : PlayerIdentity) =
    PlayerView.addOtherPlayer view player
    |> fun v -> v.otherPlayers
    |> List.length =! List.length view.otherPlayers + 1

[<Property>]
let ``After adding a player, the view contains the added player`` (view : PlayerView) (player : PlayerIdentity) =
    PlayerView.addOtherPlayer view player
    |> fun v -> v.otherPlayers
    |> List.filter (fun p -> p.identity = player)
    |> List.length >! 0

[<Property>]
let ``After adding a card to the draw deck, the size has increased by one`` (view : PlayerView) =
    PlayerView.addCardToDrawDeck view
    |> fun v -> v.drawDeckSize =! view.drawDeckSize + 1

let private countCards view =
    view.drawDeckSize
    + List.length view.self.hand
    + List.sumBy (fun playerView -> List.length playerView.hand) view.otherPlayers

[<Property>]
let ``Dealing a card to self does not change the total number of cards`` (view : PlayerView) =
    PlayerView.dealCardToSelf view |> countCards =! countCards view

[<Property>]
let ``Dealing a card to self removes a card from the draw deck`` (view : PlayerView) =
    PlayerView.dealCardToSelf view |> fun v -> v.drawDeckSize =! view.drawDeckSize - 1

[<Property>]
let ``Dealing a card to self adds a concealed card to own hand`` (view : PlayerView) =
    PlayerView.dealCardToSelf view |> fun v -> v.self.hand =! ConcealedCard :: view.self.hand

[<Property>]
let ``Dealing a card to another player does not change the total number of cards``
    (view : PlayerView)
    (card : Card)
    (player : PlayerIdentity) =

    let newView = PlayerView.dealCardToOtherPlayer view card player

    test <@ countCards newView = countCards view @>

let viewWithPlayer view player = 
    { view with
        otherPlayers = PlayerState.create player :: view.otherPlayers |> List.distinctBy (fun player -> player.identity)
    }

[<Property>]
let ``Dealing a card to another player removes a card from the draw deck``
    (view : PlayerView)
    (card : Card)
    (player : PlayerIdentity) =

    PlayerView.dealCardToOtherPlayer (viewWithPlayer view player) card player
    |> fun v -> v.drawDeckSize =! view.drawDeckSize - 1

let private getHandOfOtherPlayer view playerIdentity =
    view.otherPlayers
    |> List.filter (fun player -> player.identity = playerIdentity)
    |> List.map (fun player -> player.hand)
    |> List.tryHead

[<Property>]
let ``After dealing a card to another player the recipient has one more card in their hand``
    (view : PlayerView)
    (card : Card)
    (player : PlayerIdentity) =

    let newView = PlayerView.dealCardToOtherPlayer view card player
    getHandOfOtherPlayer newView player
    |> Option.map List.length =! (getHandOfOtherPlayer view player |> Option.map (List.length >> ((+) 1)))

[<Property>]
let ``After dealing a card to another player the recipient's hand contains the card``
    (view : PlayerView)
    (card : Card)
    (player : PlayerIdentity) =

    let newView = PlayerView.dealCardToOtherPlayer (viewWithPlayer view player) card player
    getHandOfOtherPlayer newView player |> Option.map (List.contains card) =! Some true

[<Property>]
let ``Dealing to a player who is not in the game has no affect``
    (view : PlayerView)
    (card : Card)
    (player : PlayerIdentity) =

    let viewWithoutPlayer =
        { view with otherPlayers = List.filter (fun p -> p.identity <> player) view.otherPlayers }
    
    let newView = PlayerView.dealCardToOtherPlayer viewWithoutPlayer card player

    newView =! viewWithoutPlayer
