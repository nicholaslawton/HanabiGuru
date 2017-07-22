﻿namespace HanabiGuru.Client.Console

type Command =
    | AddPlayer of string
    | StartGame

module Commands =
    open HanabiGuru.Engine

    let execute history = function
        | AddPlayer name -> Game.addPlayer (Player.create name) history
        | StartGame -> Game.startGame history
