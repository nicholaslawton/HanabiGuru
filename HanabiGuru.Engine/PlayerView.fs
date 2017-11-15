module HanabiGuru.Engine.PlayerView

let self = List.pick (function
    | SelfJoined player -> Some player
    | _ -> None)

let otherPlayers view =
    view
    |> List.choose (function
        | SelfJoined player
        | OtherPlayerJoined player -> Some player
        | _ -> None)
    |> List.splitBy ((=) (self view))
    |> List.rev
    |> List.collect id

let drawDeckSize = List.sumBy (function
    | CardAddedToDrawDeck _ -> 1
    | CardDealtToSelf _
    | CardDealtToOtherPlayer _ -> -1
    | _ -> 0)

let hand =
    List.choose (function
        | CardDealtToSelf cardKey -> ConcealedCard cardKey |> Some
        | _ -> None)

let otherHand player view =
    view
    |> List.choose (function
        | CardDealtToOtherPlayer (card, otherPlayer) when otherPlayer = player -> Some card
        | _ -> None)
    |> PlayerHand.create player

let fuseTokens _ = GameRules.fuseTokensAvailable

let clockTokens = List.sumBy (function
    | ClockTokenAdded -> 1
    | ClockTokenSpent -> -1
    | _ -> 0)

module CardIdentity =
    
    let deduce view (ConcealedCard cardKey) =
        let information =
            view
            |> List.choose (function
                | InformationReceived (key, traitMatch) when key = cardKey -> Some traitMatch
                | _ -> None)
        let candidates =
            view
            |> List.choose (function
                | CardAddedToDrawDeck card -> Some card
                | _ -> None)
            |> List.removeEach (List.choose (function
                | CardDealtToOtherPlayer ({ identity = card }, _) -> Some card
                | _ -> None) view)
            |> List.filter (fun (Card (suit, rank)) ->
                information
                |> List.exists (function
                    | Matches (SuitTrait matchingSuit) -> matchingSuit <> suit
                    | Matches (RankTrait matchingRank) -> matchingRank <> rank
                    | DoesNotMatch (SuitTrait notMatchingSuit) -> notMatchingSuit = suit
                    | DoesNotMatch (RankTrait notMatchingRank) -> notMatchingRank = rank)
                |> not)
            |> List.countBy id
        let candidatesCount = List.sumBy snd candidates

        candidates
        |> List.map (fun (card, count) -> { card = card; probability = double count / double candidatesCount })
        |> List.sortByDescending (fun candidate -> candidate.probability)
