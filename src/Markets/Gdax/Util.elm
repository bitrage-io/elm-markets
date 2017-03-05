module Markets.Gdax.Util exposing (..)

import Model.Pair as Pair exposing (Pair)


pairToString : Pair -> String
pairToString { base, quote } =
    (toString base) ++ "-" ++ (toString quote)
