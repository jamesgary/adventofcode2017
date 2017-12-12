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
import Set exposing (Set)


main : Html msg
main =
    [ """
0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5
"""
    , input
    ]
        |> List.map solve
        |> List.map (\num -> "Num: " ++ toString num)
        |> List.map (\s -> div [] [ text s ])
        |> (\ds -> div [] ds)


type alias Pipes =
    Array Int (List Int)


solve : String -> Int
solve str =
    str
        |> String.trim
        |> String.split "\n"
        |> List.map
            (\str ->
                regexSubSafetyDance "^\\d+.....(.*)$" str
                    |> String.split ","
                    |> List.map String.trim
                    |> List.map (String.toInt >> resultSafetyDance)
            )
        |> Array.fromList
        -- be lazy and get entire group for ALL ids
        -- and just uniq it later
        |> (\pipes ->
                pipes
                    |> Array.indexedMap
                        (\id ids ->
                            getGroupsForId id Set.empty pipes
                        )
           )
        |> Array.toList
        |> List.Extra.uniqueBy Set.toList
        |> List.length


solveA : String -> Int
solveA str =
    str
        |> String.trim
        |> String.split "\n"
        |> List.map
            (\str ->
                regexSubSafetyDance "^\\d+.....(.*)$" str
                    |> String.split ","
                    |> List.map String.trim
                    |> List.map (String.toInt >> resultSafetyDance)
            )
        |> Array.fromList
        |> getGroupsForId 0 Set.empty
        |> Debug.log "groups of 0"
        |> Set.size


getGroupsForId : Int -> Set Int -> Array (List Int) -> Set Int
getGroupsForId numToFind alreadyConnected pipes =
    case Array.get numToFind pipes of
        Nothing ->
            Debug.crash "Should always find something!"

        Just ids ->
            List.foldl
                (\id ac ->
                    if Set.member id ac then
                        ac
                    else
                        Set.union ac (getGroupsForId id (Set.insert id ac) pipes)
                )
                alreadyConnected
                ids



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
