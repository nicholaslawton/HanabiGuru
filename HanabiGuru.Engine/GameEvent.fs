namespace HanabiGuru.Engine

type GameEvent =
    | PlayerJoined of PlayerIdentity
    | FuseTokenAdded
    | ClockTokenAdded
    | CardAddedToDrawDeck of Card
    | CardDealtToPlayer of CardInstance * PlayerIdentity
    | StartTurn of PlayerIdentity
    | InformationGiven of CardInstanceKey * CardTraitMatch

module GameEvent =
    
    let toEventForPlayer player = function
        | PlayerJoined self when player = self ->
            SelfJoined self |> Some
        | PlayerJoined otherPlayer ->
            OtherPlayerJoined otherPlayer |> Some
        | FuseTokenAdded ->
            PlayerEvent.FuseTokenAdded |> Some
        | ClockTokenAdded ->
            PlayerEvent.ClockTokenAdded |> Some
        | CardAddedToDrawDeck card ->
            PlayerEvent.CardAddedToDrawDeck card |> Some
        | CardDealtToPlayer (card, otherPlayer) when otherPlayer <> player ->
            CardDealtToOtherPlayer (card, otherPlayer) |> Some
        | CardDealtToPlayer ({ instanceKey = cardKey }, _) ->
            CardDealtToSelf cardKey |> Some
        | StartTurn _ ->
            None
        | InformationGiven (cardKey, traitMatch) ->
            InformationReceived (cardKey, traitMatch) |> Some
