namespace HanabiGuru.Client.Console

open HanabiGuru.Engine

type Command =
    | AddPlayer of string
    | StartGame
    | GiveInformation of string * CardTrait
    | DiscardCard of char

module Command =

    let execute history = function
        | AddPlayer name -> Game.addPlayer (PlayerIdentity.create name) history
        | StartGame -> Game.startGame history
        | GiveInformation (name, cardTrait) -> Game.giveInformation (PlayerIdentity.create name) cardTrait history
        | DiscardCard _ -> Error (CannotTakeTurn [])
