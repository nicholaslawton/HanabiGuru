namespace HanabiGuru.Engine.Tests

open FsCheck
open HanabiGuru.Engine.Model

type OneOrMorePlayers = Player * Player list 
type TwoPlayers = Player * Player
 
type DistinctPlayers = 
    static member OneOrMorePlayers() = 
        Arb.generate<Player> 
        |> Gen.nonEmptyListOf 
        |> Gen.map List.distinct 
        |> Gen.filter (List.length >> ((<) 1)) 
        |> Gen.map (function
            | one :: more -> one, more
            | _ -> invalidArg "player" "At least one player required")
        |> Arb.fromGen 

    static member TwoPlayers() = 
        Arb.generate<Player> 
        |> Gen.two 
        |> Gen.filter (fun (x, y) -> x <> y)
        |> Arb.fromGen 
