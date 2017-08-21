namespace HanabiGuru.Client.Console.Tests

open System
open FsCheck
open HanabiGuru.Engine

type Name = Name of string
type Names = Names of Set<string>

type InputGeneration =
    static member private nameGenerator =
        Arb.generate<char>
        |> Gen.filter (fun c -> ' ' <= c && c <= '~')
        |> Gen.nonEmptyListOf
        |> Gen.map string
        |> Gen.filter (not << String.IsNullOrWhiteSpace)

    static member Name() =
        InputGeneration.nameGenerator
        |> Gen.map Name
        |> Arb.fromGen

    static member Names() =
        InputGeneration.nameGenerator
        |> Gen.nonEmptyListOf
        |> Gen.map set
        |> Gen.filter (Set.count >> ((<=) GameRules.minimumPlayers)) 
        |> Gen.filter (Set.count >> ((>=) GameRules.maximumPlayers)) 
        |> Gen.map Names
        |> Arb.fromGen
