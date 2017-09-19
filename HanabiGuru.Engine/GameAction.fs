module HanabiGuru.Engine.GameAction

let dealInitialHands drawDeck players =
    let playerCount = Set.count players
    let handSize = if playerCount <= 3 then 5 else 4
    players
    |> Set.toList
    |> List.replicate handSize
    |> List.collect id
    |> List.zip (drawDeck
        |> List.sortBy (ignore >> Random.double)
        |> List.take (playerCount * handSize)
        |> List.map (fun card -> CardInstance.create (CardInstance.nextInstanceKey ()) card))
