module Json.Decode.Extra exposing (..)

import Date exposing (Date)
import Math
import Json.Decode exposing (..)
import String
import Time exposing (Time)


customDecoder : Decoder b -> (b -> Result String a) -> Decoder a
customDecoder decoder toResult =
    andThen
        (\a ->
            case toResult a of
                Ok b ->
                    succeed b

                Err err ->
                    fail err
        )
        decoder


timestamp : Decoder Date
timestamp =
    customDecoder anyFloat (\i -> i * 1000 |> Date.fromTime |> Ok)


date : Decoder Date
date =
    customDecoder string Date.fromString


dateToTime : Decoder Time
dateToTime =
    date |> andThen (\d -> succeed <| Date.toTime d)


anyDate : Decoder Date
anyDate =
    oneOf [ timestamp, date ]


toBool : String -> Result String Bool
toBool str =
    let
        lowerStr =
            String.toLower str
    in
        if List.member lowerStr [ "true", "yes", "1" ] then
            Ok True
        else if List.member lowerStr [ "false", "no", "0" ] then
            Ok False
        else
            Err ("'" ++ str ++ "' is not a bool value")


boolString : Decoder Bool
boolString =
    customDecoder string toBool


anyBool : Decoder Bool
anyBool =
    oneOf [ bool, boolString ]


intString : Decoder Int
intString =
    customDecoder string String.toInt


largeInt : Decoder Int
largeInt =
    customDecoder float (\f -> f |> truncate |> Ok)


anyInt : Decoder Int
anyInt =
    oneOf [ int, intString, largeInt ]


scientificNotationString : Decoder Float
scientificNotationString =
    customDecoder string Math.fromScientificNotation


floatString : Decoder Float
floatString =
    customDecoder string String.toFloat


anyFloat : Decoder Float
anyFloat =
    oneOf [ float, floatString, scientificNotationString ]


asString : Decoder a -> (a -> String) -> Decoder String
asString decoder toStr =
    customDecoder decoder (Ok << toStr)
