module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html, div, text)
import List.Extra


vals =
    """bdwdjjo avricm cjbmj ran lmfsom ivsof
blah blah blah
jwfm ptjwrbl hhuv uolz adyweh qpj wxyogp igvnojq jmfw pqs fsnirby"""


main : Html msg
main =
    vals
        |> String.split "\n"
        |> List.filter isValidB
        |> List.length
        |> toString
        |> text


isValidB : String -> Bool
isValidB passphrase =
    let
        tokens =
            passphrase
                |> String.split " "
                |> List.map
                    (String.split "" >> List.sort)
    in
    List.length tokens == List.length (List.Extra.unique tokens)


isValidA : String -> Bool
isValidA passphrase =
    let
        tokens =
            passphrase
                |> String.split " "
    in
    List.length tokens == List.length (List.Extra.unique tokens)
