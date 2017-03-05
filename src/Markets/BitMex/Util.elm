module Markets.BitMex.Util exposing (..)

import Model.Pair as Pair exposing (Pair)
import Model.Symbol as Symbol exposing (Symbol(..))


symbolToString : Symbol -> String
symbolToString symbol =
    case symbol of
        BTC ->
            "XBT"

        _ ->
            toString symbol


pairToString : Pair -> String
pairToString { base, quote } =
    (symbolToString base) ++ (symbolToString quote)


pairFromString : String -> Result String Pair
pairFromString pairStr =
    Result.map2 Pair
        (Symbol.fromString <| String.left 3 pairStr)
        (Symbol.fromString <| String.right 3 pairStr)
