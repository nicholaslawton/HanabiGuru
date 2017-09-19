namespace HanabiGuru.Engine

open System

type Suit =
    | Blue
    | Green
    | Red
    | White
    | Yellow

type Rank = Rank of int

type Card = Card of Suit * Rank

type CardInstanceKey = CardInstanceKey of Guid

type ConcealedCard = ConcealedCard of CardInstanceKey

type CandidateIdentity =
    {
        card : Card
        probability : double
    }

type CardInstance =
    {
        instanceKey : CardInstanceKey
        identity : Card
    }

type CardTraitMatch =
    | Matches of Suit
    | DoesNotMatch of Suit

module CardInstance =

    let nextInstanceKey () = CardInstanceKey (Guid.NewGuid())

    let create key card =
        {
            instanceKey = key
            identity = card
        }