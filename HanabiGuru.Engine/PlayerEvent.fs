namespace HanabiGuru.Engine

type PlayerEvent =
    | SelfJoined of PlayerIdentity
    | OtherPlayerJoined of PlayerIdentity
    | FuseTokenAdded
    | ClockTokenAdded
    | CardAddedToDrawDeck of Card
    | CardDealtToSelf of CardInstanceKey
    | CardDealtToOtherPlayer of CardInstance * PlayerIdentity
    | InformationReceived of CardInstanceKey * CardTraitMatch
    | ClockTokenSpent
    | ClockTokenRestored
