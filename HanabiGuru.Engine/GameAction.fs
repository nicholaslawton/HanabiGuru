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
        |> List.map (fun card -> CardInstance.create (CardInstance.nextInstanceKey ()) card))
