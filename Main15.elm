module Main exposing (main)

import Array.Hamt as Array exposing (Array)
import Ascii
import Bitwise
import Dict exposing (Dict)
import EveryDict as EDict exposing (EveryDict)
import Hex
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Input exposing (input)
import KnotHash
import List.Extra
import Regex
import Set exposing (Set)


main : Html msg
main =
    div []
        --[ ( 65, 8921 )
        [ ( 618, 814 )
            |> solve
            |> format
        ]


format : Int -> Html msg
format count =
    div
        [ style [ ( "font-family", "monospace" ) ] ]
        [ div [] [ text ("Judge count: " ++ toString count) ] ]


solve : ( Int, Int ) -> Int
solve ( seedA, seedB ) =
    List.range 0 (5000000 - 1)
        |> List.foldl
            (\i ( genA, genB, count ) ->
                let
                    newGenA =
                        step genA

                    newGenB =
                        step genB

                    newCount =
                        if match newGenA.val newGenB.val then
                            count + 1
                        else
                            count

                    _ =
                        if i % 10000 == 0 then
                            Debug.log ("Iteration #" ++ toString (i // 10000) ++ " * 10000") newCount
                        else
                            0
                in
                ( newGenA, newGenB, newCount )
            )
            ( makeGenA seedA, makeGenB seedB, 0 )
        |> (\( _, _, count ) -> count)


type alias Gen =
    { factor : Int
    , val : Int
    , multBy : Int
    }


makeGenA : Int -> Gen
makeGenA val =
    { factor = 16807
    , val = val
    , multBy = 4
    }


makeGenB : Int -> Gen
makeGenB val =
    { factor = 48271
    , val = val
    , multBy = 8
    }


step : Gen -> Gen
step ({ factor, val, multBy } as gen) =
    let
        newVal =
            rem (factor * val) 2147483647
    in
    if newVal % multBy == 0 then
        { gen | val = newVal }
    else
        step { gen | val = newVal }


match : Int -> Int -> Bool
match numA numB =
    getLast16Bits numA == getLast16Bits numB


getLast16Bits : Int -> List Int
getLast16Bits num =
    num
        |> toBinary
        |> (\b -> List.append b [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ])
        |> List.take 16


toBinary : Int -> List Int
toBinary num =
    -- reversed binary list (lowest first)
    case num of
        0 ->
            [ 0 ]

        1 ->
            [ 1 ]

        _ ->
            let
                remainder =
                    rem num 2

                half =
                    num // 2
            in
            remainder :: toBinary half



-- Helpers!


concat : List (Array a) -> Array a
concat list =
    list
        |> List.map Array.toList
        |> List.concat
        |> Array.fromList


reverse : Array a -> Array a
reverse array =
    array
        |> Array.toList
        |> List.reverse
        |> Array.fromList


getIndexFor : a -> List a -> Int
getIndexFor target list =
    getIndexForHelper 0 target list


getIndexForHelper : Int -> a -> List a -> Int
getIndexForHelper indexGuess target list =
    case list of
        [] ->
            Debug.crash "Couldn't get the index in getIndexFor!"

        x :: xs ->
            if x == target then
                indexGuess
            else
                getIndexForHelper (indexGuess + 1) target xs


resultSafetyDance : Result a b -> b
resultSafetyDance result =
    case result of
        Ok b ->
            b

        Err err ->
            Debug.crash (toString err)


maybeSafetyDance : Maybe a -> a
maybeSafetyDance maybe =
    case maybe of
        Just a ->
            a

        Nothing ->
            Debug.crash "Got Nothing!"


regexSafetyDance : String -> String -> String
regexSafetyDance regex str =
    Regex.find (Regex.AtMost 1) (Regex.regex regex) str
        |> List.head
        |> maybeSafetyDance
        |> .match


regexSubSafetyDance : String -> String -> String
regexSubSafetyDance regex str =
    Regex.find (Regex.AtMost 1) (Regex.regex regex) str
        |> List.head
        |> maybeSafetyDance
        |> .submatches
        |> List.head
        |> maybeSafetyDance
        |> maybeSafetyDance
