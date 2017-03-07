module Markets
    exposing
        ( all
        , poloniex
        )

{-|

# Markets
@docs all, poloniex

-}

import Market.Api
import Market.Api.Poloniex


{-| All markets
-}
all : List Market.Api.Api
all =
    [ poloniex
    ]


{-| Poloniex
-}
poloniex : Market.Api.Api
poloniex =
    Market.Api.Poloniex.api
