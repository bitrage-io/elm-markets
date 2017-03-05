module Markets.Bitfinex.Util exposing (..)

import Model.Pair as Pair exposing (Pair)
import Model.Symbol as Symbol exposing (Symbol(..))


pairToString : Pair -> String
pairToString { base, quote } =
    (toString base) ++ (toString quote)


pairFromString : String -> Result String Pair
pairFromString pairStr =
    Result.map2 Pair
        (Symbol.fromString <| String.left 3 pairStr)
        (Symbol.fromString <| String.right 3 pairStr)
