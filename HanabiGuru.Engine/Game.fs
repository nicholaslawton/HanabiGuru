module HanabiGuru.Engine.Game

open HanabiGuru.Engine

let addPlayer history player = PlayerJoined player :: history
