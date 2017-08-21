namespace HanabiGuru.Engine

type PlayerIdentity = Name of string

type PlayerHand =
    {
        player : PlayerIdentity
        cards : Card list
    }

module PlayerIdentity =

    let create name = Name name

module PlayerHand =
    
    let create player cards =
        {
            player = player
            cards = cards
        }
