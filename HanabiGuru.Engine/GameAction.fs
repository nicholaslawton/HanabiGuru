module HanabiGuru.Engine.GameAction

let dealInitialHands drawDeck players =
    let playerCount = List.length players
    let handSize = if playerCount <= 3 then 5 else 4
    players
    |> List.replicate handSize
    |> List.collect id
    |> List.zip (drawDeck
        |> List.sortBy (ignore >> Random.double)
        |> List.take (playerCount * handSize)
        |> List.map (fun card -> CardInstance.create card))

let draw = function
    | [] -> None
    | drawDeck -> drawDeck |> List.randomItem (Random.int) |> CardInstance.create |> Some

let nextPlayer players activePlayer =
    Seq.initInfinite (fun _ -> players)
    |> Seq.collect id
    |> Seq.skipWhile ((<>) activePlayer)
    |> Seq.skip 1
    |> Seq.head

let cardMatch cardTrait { instanceKey = key; identity = Card (suit, rank) } =
    let matchType =
        if SuitTrait suit = cardTrait || RankTrait rank = cardTrait
        then Matches
        else DoesNotMatch
    CardInformation (key, matchType cardTrait)
