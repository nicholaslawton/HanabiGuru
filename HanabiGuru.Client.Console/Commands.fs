namespace HanabiGuru.Client.Console

type Command =
    | AddPlayer of string

module Commands =
    open HanabiGuru.Engine

    let execute errorHandler history (AddPlayer name) =
        let newHistory, result = Game.addPlayer EventHistory.recordEvent Game.canAddPlayer history (Player.create name)
        match result with
        | CannotAddPlayer reason -> errorHandler reason
        | PlayerAdded -> ()
        newHistory
