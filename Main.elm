module Main exposing (main)

--import Dict exposing (Dict)

import Array.Hamt as Array exposing (Array)
import Ascii
import Bitwise
import EveryDict as EDict exposing (EveryDict)
import Hex
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Input exposing (input)
import List.Extra
import Regex


type alias Dirs =
    EveryDict Dir Int


main : Html msg
main =
    [ """ne,ne,ne"""
    , """ne,ne,sw,sw"""
    , """ne,ne,s,s"""
    , """se,sw,se,sw,sw"""
    , input
    ]
        |> List.map solve
        |> List.map
            (\( dist, maxDist ) ->
                "Distance: "
                    ++ toString dist
                    ++ ", Max Distance: "
                    ++ toString maxDist
            )
        |> List.map (\s -> div [] [ text s ])
        |> (\ds -> div [] ds)


solve : String -> ( Int, Int )
solve str =
    str
        |> String.trim
        |> String.split ","
        |> List.map toDir
        |> List.foldl addDir ( initDir, 0 )
        |> (\( dirs, maxDist ) ->
                ( totalDist dirs, maxDist )
           )


totalDist : Dirs -> Int
totalDist dirs =
    let
        ups =
            (2 * getDirCount N dirs)
                + (getDirCount NE dirs + getDirCount NW dirs)

        downs =
            (2 * getDirCount S dirs)
                + (getDirCount SE dirs + getDirCount SW dirs)

        lefts =
            getDirCount NW dirs + getDirCount SW dirs

        rights =
            getDirCount NE dirs + getDirCount SE dirs
    in
    (abs (ups - downs) + abs (lefts - rights)) // 2


initDir : Dirs
initDir =
    List.foldl (\d -> EDict.insert d 0) EDict.empty allDirs


addDir : Dir -> ( Dirs, Int ) -> ( Dirs, Int )
addDir dir ( dirs, maxDist ) =
    let
        newDirs =
            EDict.update dir (\mn -> Maybe.map (\n -> n + 1) mn) dirs
    in
    ( newDirs, max maxDist (totalDist newDirs) )


getDirCount : Dir -> Dirs -> Int
getDirCount dir dirs =
    dirs
        |> EDict.get dir
        |> maybeSafetyDance


type Dir
    = NE
    | SE
    | S
    | SW
    | NW
    | N


allDirs =
    [ NE, SE, S, SW, NW, N ]


toDir : String -> Dir
toDir str =
    case str of
        "ne" ->
            NE

        "se" ->
            SE

        "s" ->
            S

        "sw" ->
            SW

        "nw" ->
            NW

        "n" ->
            N

        _ ->
            Debug.crash "bad dir!" str



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
