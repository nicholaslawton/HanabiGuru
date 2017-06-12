namespace HanabiGuru.Client.Console

type Command =
    | AddPlayer of string

module Commands =
    open HanabiGuru.Engine

    let execute history (AddPlayer name) = Game.addPlayer Game.canAddPlayer history (Player.create name)
