module Market.Api.Gdax.Util exposing (..)

import Market


pairToString : Market.Pair -> String
pairToString { base, quote } =
    (toString base) ++ "-" ++ (toString quote)
