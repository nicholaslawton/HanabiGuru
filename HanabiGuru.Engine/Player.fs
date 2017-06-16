namespace HanabiGuru.Engine

type PlayerIdentity = { name : string }

type Player =
    {
        identity : PlayerIdentity
        hand : Card list
    }

module Player =
    
    let create name =
        {
            identity = { name = name }
            hand = []
        }
