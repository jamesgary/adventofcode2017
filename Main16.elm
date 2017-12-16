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


billion =
    100



--(1000 * 1000 * 1000) % 36


heartbeat =
    1000



--* 10


main : Html msg
main =
    div []
        [ let
            danceMoves =
                dmsFromStr
                    --"s1,x3/4,pe/b"
                    input

            dancedPartners =
                dance danceMoves initPartners
                    |> Debug.log "dancedPartners"

            {- thought i'd need this code...

               danceMap =
                   dancedPartners
                       |> Array.map
                           (\partner ->
                               initPartners
                                   |> Array.toList
                                   |> List.indexedMap
                                       (\i p ->
                                           if p == partner then
                                               Just i
                                           else
                                               Nothing
                                       )
                                   |> List.filterMap identity
                                   |> List.head
                                   |> maybeSafetyDance
                           )
                       |> Debug.log "danceMap"

               quickDance partners =
                   partners
                       |> Array.indexedMap
                           (\i p ->
                               let
                                   partnerIndexToPluckFrom =
                                       Array.get i danceMap |> maybeSafetyDance
                               in
                               Array.get partnerIndexToPluckFrom partners
                                   |> maybeSafetyDance
                           )
            -}
            bilDancedPartners =
                List.range 0 billion
                    |> List.foldl
                        (\i partners ->
                            let
                                _ =
                                    Debug.log "p,i" ( partners |> Array.toList |> String.fromList, i )
                            in
                            dance danceMoves partners
                        )
                        initPartners

            _ =
                Debug.log "P" (bilDancedPartners |> Array.toList |> String.fromList)
          in
          bilDancedPartners
            |> Array.toList
            |> String.fromList
            |> text
        ]


type DanceMove
    = Spin Int
    | Exchange Int Int
    | Partner Char Char


dmsFromStr : String -> List DanceMove
dmsFromStr dmsStr =
    dmsStr
        |> String.split ","
        |> List.map
            (\dmStr ->
                case regexSafetyDance "^\\w" dmStr of
                    "s" ->
                        Spin
                            (regexSubSafetyDance "^s(\\d+)" dmStr
                                |> String.toInt
                                |> resultSafetyDance
                                |> (\pivot -> numPartners - pivot)
                            )

                    "x" ->
                        Exchange
                            (regexSubSafetyDance "^x(\\w+)/" dmStr
                                |> String.toInt
                                |> resultSafetyDance
                            )
                            (regexSubSafetyDance "^x\\w+/(\\w+)$" dmStr
                                |> String.toInt
                                |> resultSafetyDance
                            )

                    "p" ->
                        Partner
                            (regexSubSafetyDance "^p(\\w)/" dmStr
                                |> String.toList
                                |> List.head
                                |> maybeSafetyDance
                            )
                            (regexSubSafetyDance "^p\\w/(\\w)$" dmStr
                                |> String.toList
                                |> List.head
                                |> maybeSafetyDance
                            )

                    _ ->
                        Debug.crash "Bad dance move regex!"
            )


format : String -> Html msg
format partners =
    div [ style [ ( "margin", "16px" ), ( "font-family", "monospace" ) ] ]
        [ div [] [ text ("Partners: " ++ partners) ] ]


initPartners : Array Char
initPartners =
    --"abcde"
    "abcdefghijklmnop"
        |> String.toList
        |> Array.fromList


numPartners =
    --5
    16


dance : List DanceMove -> Array Char -> Array Char
dance danceMoves inputPartners =
    danceMoves
        |> List.foldl
            (\danceMove partners ->
                case danceMove of
                    Spin pivot ->
                        Array.append
                            (Array.slice pivot numPartners partners)
                            (Array.slice 0 pivot partners)

                    Exchange posA posB ->
                        let
                            valA =
                                Array.get posA partners |> maybeSafetyDance

                            valB =
                                Array.get posB partners |> maybeSafetyDance
                        in
                        partners
                            |> Array.set posA valB
                            |> Array.set posB valA

                    Partner partnerA partnerB ->
                        partners
                            |> Array.map
                                (\partner ->
                                    if partner == partnerA then
                                        partnerB
                                    else if partner == partnerB then
                                        partnerA
                                    else
                                        partner
                                )
            )
            inputPartners



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
