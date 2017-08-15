namespace HanabiGuru.Engine

type PlayerEvent =
    | SelfJoined of PlayerIdentity
    | OtherPlayerJoined of PlayerIdentity
    | FuseTokenAdded
    | ClockTokenAdded
    | CardAddedToDrawDeck
    | CardDealtToSelf
    | CardDealtToOtherPlayer of Card * PlayerIdentity
