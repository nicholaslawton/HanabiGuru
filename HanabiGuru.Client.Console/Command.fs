namespace HanabiGuru.Client.Console

open HanabiGuru.Engine

type Command =
    | AddPlayer of string
    | StartGame
    | GiveInformation of string * CardTrait

module Command =
    open HanabiGuru.Engine

    let execute history = function
        | AddPlayer name -> Game.addPlayer (PlayerIdentity.create name) history
        | StartGame -> Game.startGame history
        | GiveInformation (name, cardTrait) -> Game.giveInformation (PlayerIdentity.create name) cardTrait history
