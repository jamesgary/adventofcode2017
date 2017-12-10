module Main exposing (answer, main)

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Input exposing (input)
import List.Extra
import Regex


testInput =
    """94,84,0,79,2,27,81,1,123,93,218,23,103,255,254,243"""


max =
    --4
    255


main : Html msg
main =
    div []
        [ div [ style [ ( "font-family", "monospace" ) ] ] [ text answer ]
        , div [ style [ ( "font-family", "monospace" ) ] ] [ text "0 1 2 3 4" ]
        , div [ style [ ( "font-family", "monospace" ) ] ] [ text "2 1 0 3 4" ]
        , div [ style [ ( "font-family", "monospace" ) ] ] [ text "4 3 0 1 2" ]
        , div [ style [ ( "font-family", "monospace" ) ] ] [ text "3 4 2 1 0" ]
        ]


answer : String
answer =
    --"3,4,1,5"
    testInput
        |> String.trim
        |> String.split ","
        |> List.map (String.toInt >> resultSafetyDance)
        |> List.foldl tie ( 0, 0, List.range 0 max |> Array.fromList )
        |> toString


tie : Int -> ( Int, Int, Array Int ) -> ( Int, Int, Array Int )
tie length ( startPos, skipLength, list ) =
    if length > 0 then
        let
            _ =
                Debug.log "INPUT=============" ( length, ( startPos, skipLength, list ) )

            listLength =
                Array.length list

            endPos =
                startPos + length

            ( nextStartPos, newList ) =
                if endPos < listLength then
                    ( endPos
                    , concat
                        [ Array.slice 0 startPos list
                        , reverse (Array.slice startPos endPos list)
                        , Array.slice endPos listLength list
                        ]
                    )
                else
                    let
                        fixedEndPos =
                            endPos
                                - listLength
                                |> Debug.log "fixedEndPos"

                        reversedRange =
                            concat
                                [ Array.slice startPos listLength list
                                , Array.slice 0 (fixedEndPos - 0) list
                                ]
                                |> Debug.log "range"
                                |> reverse
                                |> Debug.log "reversedRange"

                        rangeLength =
                            Array.length reversedRange
                    in
                    ( fixedEndPos
                    , concat
                        [ Array.slice (length - fixedEndPos) rangeLength reversedRange
                            |> Debug.log "FIRST"
                        , Array.slice (fixedEndPos - 0) startPos list
                            |> Debug.log "SECOND"
                        , Array.slice 0 (length - fixedEndPos) reversedRange
                            |> Debug.log "THIRD"
                        ]
                    )
        in
        ( if nextStartPos + skipLength < listLength then
            nextStartPos + skipLength
          else
            nextStartPos + skipLength - listLength
        , skipLength + 1
        , newList
        )
            |> Debug.log "result-----------------------"
    else
        ( if startPos + skipLength < Array.length list then
            startPos + skipLength
          else
            startPos + skipLength - Array.length list
        , skipLength + 1
        , list
        )


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



-- Helpers!


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
