module HanabiGuru.Engine.GameAction

let dealInitialHands drawDeck players =
    let playerCount = Set.count players
    let handSize = if playerCount <= 3 then 5 else 4
    players
    |> Set.toList
    |> List.replicate handSize
    |> List.collect id
    |> List.zip (drawDeck |> List.take (playerCount * handSize))
