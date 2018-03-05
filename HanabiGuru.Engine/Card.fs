namespace HanabiGuru.Engine

open System

type Suit =
    | Blue
    | Green
    | Red
    | White
    | Yellow

module Suit =
    let allSuits = List.sort [Blue; Green; Red; White; Yellow]

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

type CardTrait =
    | SuitTrait of Suit
    | RankTrait of Rank

type CardTraitMatch =
    | Matches of CardTrait
    | DoesNotMatch of CardTrait

type CardInformation = CardInformation of CardInstanceKey * CardTraitMatch

module CardInstance =

    let nextInstanceKey () = CardInstanceKey (Guid.NewGuid())

    let create card =
        {
            instanceKey = nextInstanceKey ()
            identity = card
        }