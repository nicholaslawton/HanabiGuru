namespace HanabiGuru.Engine

type PlayerIdentity = { name : string }

type PlayerState =
    {
        identity : PlayerIdentity
        hand : Card list
    }

module PlayerIdentity =

    let create name =  { name = name }

module PlayerState =
    
    let create identity =
        {
            identity = identity
            hand = []
        }
