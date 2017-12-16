module KnotHash exposing (hash)

import Array.Hamt as Array exposing (Array)
import Ascii
import Bitwise
import Dict exposing (Dict)
import Hex
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Input exposing (input)
import List.Extra
import Regex


hash : String -> String
hash input =
    input
        |> String.trim
        |> Ascii.fromString
        |> (\lengths -> lengths ++ [ 17, 31, 73, 47, 23 ])
        |> List.repeat 64
        |> List.concat
        |> List.foldl tie ( 0, 0, List.range 0 255 |> Array.fromList )
        |> (\( _, _, hash ) -> hash)
        |> Array.toList
        |> toSixteenChunks
        |> List.map bitwiseNums
        |> List.map toHexadecimal
        |> String.concat


toHexadecimal : Int -> String
toHexadecimal num =
    Hex.toString num
        |> (\h ->
                if String.length h == 1 then
                    "0" ++ h
                else
                    h
           )


bitwiseNums : List Int -> Int
bitwiseNums nums =
    nums
        |> List.Extra.foldl1 Bitwise.xor
        |> maybeSafetyDance


toSixteenChunks : List a -> List (List a)
toSixteenChunks list =
    List.range 0 15
        |> List.map
            (\n ->
                list
                    |> List.drop (n * 16)
                    |> List.take 16
            )


tie : Int -> ( Int, Int, Array Int ) -> ( Int, Int, Array Int )
tie length ( startPos, skipLength, list ) =
    let
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
                        endPos - listLength

                    reversedRange =
                        concat
                            [ Array.slice startPos listLength list
                            , Array.slice 0 fixedEndPos list
                            ]
                            |> reverse
                in
                ( fixedEndPos
                , concat
                    [ Array.slice (length - fixedEndPos) length reversedRange
                    , Array.slice fixedEndPos startPos list
                    , Array.slice 0 (length - fixedEndPos) reversedRange
                    ]
                )
    in
    ( if nextStartPos + skipLength <= listLength then
        nextStartPos + skipLength
      else
        (nextStartPos + skipLength) % listLength
    , skipLength + 1
    , newList
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
