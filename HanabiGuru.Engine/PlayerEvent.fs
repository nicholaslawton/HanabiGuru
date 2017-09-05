namespace HanabiGuru.Engine

type PlayerEvent =
    | SelfJoined of PlayerIdentity
    | OtherPlayerJoined of PlayerIdentity
    | FuseTokenAdded
    | ClockTokenAdded
    | CardAddedToDrawDeck of Card
    | CardDealtToSelf
    | CardDealtToOtherPlayer of Card * PlayerIdentity
