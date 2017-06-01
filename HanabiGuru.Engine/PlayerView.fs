namespace HanabiGuru.Engine

open Model

type PlayerView = { otherPlayers : Player list }

module PlayerView =

    let addOtherPlayer view player = { otherPlayers = player :: view.otherPlayers }
