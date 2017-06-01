module HanabiGuru.Engine.Game

open HanabiGuru.Engine

let addPlayer history player = EventHistory.recordEvent history (PlayerJoined player)
