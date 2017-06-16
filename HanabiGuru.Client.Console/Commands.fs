namespace HanabiGuru.Client.Console

type Command =
    | AddPlayer of string
    | StartGame

module Commands =
    open HanabiGuru.Engine

    let execute history = function
        | AddPlayer name -> Game.addPlayer Game.canAddPlayer history (Player.create name)
        | StartGame -> Game.prepareDrawDeck history
