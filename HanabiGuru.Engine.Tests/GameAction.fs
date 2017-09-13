module HanabiGuru.Engine.Tests.GameAction

let perform historyOrError action = Result.bind action historyOrError
