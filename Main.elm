module Main exposing (main)

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Input exposing (input)
import List.Extra
import Regex


testInput =
    """pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"""


type alias Disc =
    { name : String
    , weight : Int
    , discs : List String
    }


main : Html msg
main =
    input
        --testInput
        |> String.split "\n"
        |> List.map discFromStr
        |> findOffbalance
        |> toString
        |> text


findOffbalance : List Disc -> Int
findOffbalance discs =
    discs
        |> List.filterMap (findOffbalanceForDisc discs)
        -- might be buggy, got more than one
        |> Debug.log "off-balances"
        |> List.head
        |> maybeSafetyDance


findOffbalanceForDisc : List Disc -> Disc -> Maybe Int
findOffbalanceForDisc discs targetDisc =
    targetDisc
        |> .discs
        |> List.map (getTotalWeights discs)
        |> List.sortBy (\( a, b ) -> a + b)
        |> (\sortedWeights ->
                case ( sortedWeights, List.reverse sortedWeights ) of
                    -- assuming middle tower is "correct"
                    ( ( aDisc, aDiscs ) :: ( bDisc, bDiscs ) :: xs, ( zDisc, zDiscs ) :: ys ) ->
                        let
                            aWeight =
                                aDisc
                                    + aDiscs
                                    |> Debug.log "a"

                            bWeight =
                                bDisc
                                    + bDiscs
                                    |> Debug.log "b"

                            zWeight =
                                zDisc
                                    + zDiscs
                                    |> Debug.log "z"
                        in
                        if aWeight /= bWeight then
                            Just (aDisc + bWeight - aWeight)
                        else if zWeight /= bWeight then
                            Just (zDisc + bWeight - zWeight)
                        else
                            Nothing

                    _ ->
                        Nothing
           )


getTotalWeights : List Disc -> String -> ( Int, Int )
getTotalWeights discs name =
    let
        disc =
            List.Extra.find (\disc -> disc.name == name) discs
                |> maybeSafetyDance
    in
    ( disc.weight
    , disc.discs
        |> List.map (getTotalWeights discs)
        |> List.map (\( a, b ) -> a + b)
        |> List.sum
    )



-- grab a random disc and keep digging for it's parent?
-- pretty slow, but (shrug)


getBottomName : List Disc -> String
getBottomName discs =
    case discs of
        x :: xs ->
            findBottomest xs x.name

        _ ->
            Debug.crash "No discs?"


findBottomest : List Disc -> String -> String
findBottomest discs string =
    case
        discs
            |> List.Extra.find (\disc -> List.member string disc.discs)
    of
        Nothing ->
            string

        Just disc ->
            disc.name
                |> findBottomest discs


discFromStr : String -> Disc
discFromStr str =
    let
        name =
            str
                |> regexSafetyDance "^(\\w+)"

        --|> Debug.log "name"
        weight =
            str
                |> regexSafetyDance "\\d+"
                |> String.toInt
                |> resultSafetyDance

        --|> Debug.log "weight"
        discs =
            str
                |> Regex.find (Regex.AtMost 1) (Regex.regex "-> (.+)")
                |> List.head
                |> (\maybe ->
                        case maybe of
                            Nothing ->
                                []

                            Just match ->
                                match
                                    |> .submatches
                                    |> List.head
                                    |> (\maybeSubmatch ->
                                            case maybeSubmatch of
                                                Nothing ->
                                                    []

                                                Just submatch ->
                                                    submatch
                                                        --|> Debug.log "submatch"
                                                        |> Maybe.withDefault ""
                                                        |> String.split ", "
                                       )
                   )

        --|> Debug.log "discs"
    in
    { name = name
    , weight = weight
    , discs = discs
    }



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
