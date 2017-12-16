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
        [ ( 65, 8921 )
            --[ ( 618, 814 )
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
    List.range 0 (40000000 - 1)
        |> List.foldl
            (\i ( genA, genB, count ) ->
                let
                    newGenA =
                        step genA

                    --|> Debug.log "genA"
                    newGenB =
                        step genB

                    --|> Debug.log "genB"
                    newCount =
                        if match newGenA.val newGenB.val then
                            count + 1
                        else
                            count

                    _ =
                        if i % 100000 == 0 then
                            Debug.log ("Interation #" ++ toString (i // 100000) ++ " * 100000") ""
                        else
                            ""
                in
                ( newGenA, newGenB, newCount )
            )
            ( makeGenA seedA, makeGenB seedB, 0 )
        |> (\( _, _, count ) -> count)


type alias Gen =
    { factor : Int
    , val : Int
    }


makeGenA : Int -> Gen
makeGenA val =
    { factor = 16807
    , val = val
    }


makeGenB : Int -> Gen
makeGenB val =
    { factor = 48271
    , val = val
    }


step : Gen -> Gen
step ({ factor, val } as gen) =
    { gen | val = rem (factor * val) 2147483647 }


match : Int -> Int -> Bool
match numA numB =
    getLast16Bits numA == getLast16Bits numB


getLast16Bits : Int -> List Int
getLast16Bits num =
    num
        |> toBinary
        |> (\b -> List.append b [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ])
        |> List.take 16



--|> Debug.log "bin"
-- reversed binary list (lowest first)


toBinary : Int -> List Int
toBinary num =
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
