module Main exposing (main)

--import Dict exposing (Dict)

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
        ([ --"flqrgnkx"
           "ffayrhll"
         ]
            |> List.map solve
            |> List.map format
        )


size =
    128


sizeMinusOne =
    127


format : ( List String, Int, Int ) -> Html msg
format ( outputList, usedSquares, regions ) =
    div
        [ style [ ( "font-family", "monospace" ) ] ]
        (div [] [ text ("Used Squares: " ++ toString usedSquares) ]
            :: div [] [ text ("Regions: " ++ toString regions) ]
            :: div [] []
            :: List.map (\s -> div [] [ text s ]) outputList
        )


solve : String -> ( List String, Int, Int )
solve str =
    List.range 0 sizeMinusOne
        |> List.map
            (\i ->
                str
                    ++ "-"
                    ++ toString i
                    |> KnotHash.hash
                    |> String.toList
                    |> List.map hexadecimalToBinary
                    |> String.concat
            )
        |> (\outputList ->
                ( outputList
                , outputList
                    |> List.map
                        (\row ->
                            row
                                |> String.toList
                                |> List.map
                                    (String.fromChar >> String.toInt >> resultSafetyDance)
                                |> List.sum
                        )
                    |> List.sum
                , countRegions outputList
                )
           )


countRegions : List String -> Int
countRegions grid =
    List.range 0 sizeMinusOne
        |> List.map
            (\x ->
                List.range 0 sizeMinusOne
                    |> List.map
                        (\y ->
                            ( ( x, y )
                            , grid
                                |> List.Extra.getAt y
                                |> maybeSafetyDance
                                |> String.split ""
                                |> List.Extra.getAt x
                                |> maybeSafetyDance
                            )
                        )
            )
        |> List.concat
        |> Dict.fromList
        |> countRegionsFromGrid


countRegionsFromGrid : Dict ( Int, Int ) String -> Int
countRegionsFromGrid grid =
    countRegionsFromGridHelper grid Set.empty ( 0, 0 ) 0


countRegionsFromGridHelper :
    Dict ( Int, Int ) String
    -> Set ( Int, Int )
    -> ( Int, Int )
    -> Int
    -> Int
countRegionsFromGridHelper grid foundCoordinates ( x, y ) numRegions =
    let
        _ =
            Debug.log "curPos" ( x, y )
    in
    if y == size then
        numRegions
    else if x == size then
        countRegionsFromGridHelper grid foundCoordinates ( 0, y + 1 ) numRegions
    else if Set.member ( x, y ) foundCoordinates then
        countRegionsFromGridHelper grid foundCoordinates ( x + 1, y ) numRegions
    else
        let
            newCoordinates =
                getRegion grid ( x, y )
        in
        countRegionsFromGridHelper
            grid
            (Set.union newCoordinates foundCoordinates)
            ( x + 1, y )
            (if Set.isEmpty newCoordinates then
                numRegions
             else
                numRegions + 1
            )


getRegion : Dict ( Int, Int ) String -> ( Int, Int ) -> Set ( Int, Int )
getRegion grid curPos =
    case Dict.get curPos grid of
        Just "1" ->
            getRegionHelper grid curPos Set.empty

        _ ->
            Set.empty


getRegionHelper : Dict ( Int, Int ) String -> ( Int, Int ) -> Set ( Int, Int ) -> Set ( Int, Int )
getRegionHelper grid curPos regionCoordinates =
    case Dict.get curPos grid |> maybeSafetyDance of
        "0" ->
            regionCoordinates

        "1" ->
            if Set.member curPos regionCoordinates then
                -- no need to keep going
                regionCoordinates
            else
                curPos
                    |> getNeighborCoords
                    |> List.foldl
                        (\coord rcs ->
                            if Set.member coord rcs then
                                rcs
                            else
                                getRegionHelper grid coord rcs
                        )
                        (Set.insert curPos regionCoordinates)

        _ ->
            Debug.crash "Got neither 0 nor 1!"


getNeighborCoords : ( Int, Int ) -> List ( Int, Int )
getNeighborCoords ( x, y ) =
    let
        left =
            if x > 0 then
                Just ( x - 1, y )
            else
                Nothing

        right =
            if x < sizeMinusOne then
                Just ( x + 1, y )
            else
                Nothing

        top =
            if y > 0 then
                Just ( x, y - 1 )
            else
                Nothing

        bottom =
            if y < sizeMinusOne then
                Just ( x, y + 1 )
            else
                Nothing
    in
    [ top
    , bottom
    , left
    , right
    ]
        |> List.filterMap identity


hexadecimalToBinary : Char -> String
hexadecimalToBinary char =
    case char of
        '0' ->
            "0000"

        '1' ->
            "0001"

        '2' ->
            "0010"

        '3' ->
            "0011"

        '4' ->
            "0100"

        '5' ->
            "0101"

        '6' ->
            "0110"

        '7' ->
            "0111"

        '8' ->
            "1000"

        '9' ->
            "1001"

        'a' ->
            "1010"

        'b' ->
            "1011"

        'c' ->
            "1100"

        'd' ->
            "1101"

        'e' ->
            "1110"

        'f' ->
            "1111"

        _ ->
            Debug.crash "Got an un-hexy char!"



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
