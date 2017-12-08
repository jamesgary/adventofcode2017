module Main exposing (main)

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import List.Extra


vals =
    --    "0 2 7 0"
    "11 11 13 7 0 15 5 5 4 4 1 1 7 1 15 11"


main : Html msg
main =
    vals
        |> String.words
        |> List.map (String.toInt >> Result.withDefault -99999999)
        |> solve []
        |> toString
        |> text


solve : List (List Int) -> List Int -> Int
solve pastBanks banks =
    if List.member banks pastBanks then
        let
            --_ =
            --    Debug.log "banks" banks
            --_ =
            --    Debug.log "pastBanks" pastBanks
            _ =
                Debug.log
                    "Challenge B answer"
                    (1 + getIndexFor banks pastBanks)
        in
        0
    else
        -- redistribute!
        1 + solve (banks :: pastBanks) (redistribute banks)


redistribute : List Int -> List Int
redistribute banks =
    let
        max =
            List.maximum banks |> Maybe.withDefault 999999

        maxIndex =
            getIndexFor max banks

        arrBanks =
            banks
                |> Array.fromList
                |> Array.set maxIndex 0
    in
    redistributeHelper (maxIndex + 1) max arrBanks
        |> Array.toList


redistributeHelper : Int -> Int -> Array Int -> Array Int
redistributeHelper index num banks =
    if num == 0 then
        banks
    else
        let
            roundedIndex =
                if index < Array.length banks then
                    index
                else
                    0

            val =
                Array.get roundedIndex banks |> Maybe.withDefault 99999999

            newBanks =
                Array.set roundedIndex (val + 1) banks
        in
        redistributeHelper
            (roundedIndex + 1)
            (num - 1)
            newBanks


getIndexFor : a -> List a -> Int
getIndexFor target list =
    getIndexForHelper 0 target list


getIndexForHelper : Int -> a -> List a -> Int
getIndexForHelper indexGuess target list =
    case list of
        [] ->
            Debug.crash "couldn't refind the max??"

        x :: xs ->
            if x == target then
                indexGuess
            else
                getIndexForHelper (indexGuess + 1) target xs
