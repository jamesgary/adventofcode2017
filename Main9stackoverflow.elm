module Main exposing (answer, main)

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Input exposing (input)
import List.Extra
import Regex


testInput =
    """
   {{<!!>},{<!!>},{<!!>},{<!!>}}
"""



{-
   {}, score of 1.
   {{{}}}, score of 1 + 2 + 3 = 6.
   {{},{}}, score of 1 + 2 + 2 = 5.
   {{{},{},{{}}}}, score of 1 + 2 + 3 + 3 + 3 + 4 = 16.
   {<a>,<a>,<a>,<a>}, score of 1.
   {{<ab>},{<ab>},{<ab>},{<ab>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
   {{<!!>},{<!!>},{<!!>},{<!!>}}, score of 1 + 2 + 2 + 2 + 2 = 9.
   {{<a!>},{<a!>},{<a!>},{<ab>}}, score of 1 + 2 = 3.
-}


type Group
    = Group { groups : List Group }


main : Html msg
main =
    div [ style [ ( "font-family", "monospace" ) ] ] [ text answer ]


answer : String
answer =
    input
        --testInput
        |> String.trim
        |> String.split "\n"
        |> String.concat
        |> String.toList
        |> removeBangs
        --|> removeGarbage
        --|> removeCommas
        --|> toGroup
        |> String.fromList


toGroup : String -> Group
toGroup str =
    -- assuming that initial string will always be one valid group
    str
        |> String.dropLeft 1
        |> String.dropRight 1
        |> (\tstr -> Group { groups = toGroupContents tstr })


toGroupContents : String -> List Group
toGroupContents str =
    case String.split "" str of
        [] ->
            []

        "{" :: "}" :: xs ->
            []

        --Group { groups = [] } :: toGroupContents xs
        "{" :: b :: xs ->
            []

        --Group { groups = toGroupContents }
        _ ->
            []


removeGarbage : String -> String
removeGarbage str =
    case String.split "" str of
        "<" :: xs ->
            removeGarbage (postGarbage (String.concat xs))

        a :: xs ->
            a ++ removeGarbage (String.concat xs)

        _ ->
            str


postGarbage : String -> String
postGarbage str =
    case String.split "" str of
        [] ->
            Debug.crash "Unclosed garbage!"

        ">" :: xs ->
            String.concat xs

        _ :: xs ->
            String.concat xs


removeBangs : List Char -> List Char
removeBangs str =
    case str of
        '!' :: _ :: xs ->
            removeBangs xs

        a :: xs ->
            a :: removeBangs xs

        _ ->
            str


removeCommas : String -> String
removeCommas str =
    str
        |> String.filter (\c -> c /= ',')



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
