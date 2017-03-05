module Math exposing (..)

import Regex exposing (Regex, HowMany(..))


scientificNotationRegex : Regex
scientificNotationRegex =
    Regex.regex "([\\d\\.]+)[eE]-?([\\d\\.]+)"


fromScientificNotation : String -> Result String Float
fromScientificNotation maybeNum =
    let
        maybeBaseExp =
            maybeNum
                |> Regex.find (AtMost 1) scientificNotationRegex
                |> List.head
                |> Maybe.andThen (\match -> Just match.submatches)
                |> Maybe.andThen
                    (\submatches ->
                        case submatches of
                            [ Just baseStr, Just expStr ] ->
                                Just ( baseStr, expStr )

                            _ ->
                                Nothing
                    )
                |> Maybe.andThen
                    (\( baseStr, expStr ) ->
                        case ( String.toFloat baseStr, String.toFloat expStr ) of
                            ( Ok base, Ok exp ) ->
                                Just ( base, exp )

                            _ ->
                                Nothing
                    )
    in
        case maybeBaseExp of
            Just ( base, exp ) ->
                Ok <| base * 10 ^ exp

            _ ->
                Err <| "'" ++ maybeNum ++ "' could not be converted from scientific notation"
