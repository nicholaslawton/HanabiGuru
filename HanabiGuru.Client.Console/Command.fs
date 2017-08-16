namespace HanabiGuru.Client.Console

type Command =
    | AddPlayer of string
    | StartGame

module Command =
    open HanabiGuru.Engine

    let execute history = function
        | AddPlayer name -> Game.addPlayer (PlayerIdentity.create name) history
        | StartGame -> Game.startGame history
