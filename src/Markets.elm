module Markets
    exposing
        ( all
        , btce
        , browserSafe
        , poloniex
        )

{-|

# Markets
@docs all, browserSafe, btce, poloniex

-}

import Market.Api
import Market.Api.Btce
import Market.Api.Gdax
import Market.Api.Poloniex


{-| All markets
-}
all : List Market.Api.Api
all =
    [ btce
    , poloniex
    ]


{-| Markets that enable CORS
-}
browserSafe : List Market.Api.Api
browserSafe =
    [ poloniex
    ]


{-| Btce
-}
btce : Market.Api.Api
btce =
    Market.Api.Btce.api


{-| Poloniex
-}
gdax : Market.Api.Api
gdax =
    Market.Api.Gdax.api


{-| Poloniex
-}
poloniex : Market.Api.Api
poloniex =
    Market.Api.Poloniex.api
