module Markets.Bitstamp.PairList exposing (..)

import Model.Pair exposing (Pair)
import Model.Symbol exposing (Symbol(..))


pairs : List Pair
pairs =
    [ { base = BTC, quote = USD }
    , { base = BTC, quote = EUR }
    ]
