module Main exposing (main)

import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Input exposing (input)
import List.Extra
import Regex


testInput =
    """b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10"""


main : Html msg
main =
    input
        --testInput
        |> String.split "\n"
        |> List.map instFromStr
        --|> Debug.log "INSTS"
        |> List.foldl applyInst ( Dict.empty, 0 )
        |> (\( register, max ) ->
                ( register |> Dict.values |> List.maximum
                , max
                )
           )
        |> toString
        |> text


type alias Inst =
    { reg : String
    , op : Op
    , amt : Int
    , compReg : String
    , compOp : CompOp
    , compVal : Int
    }


type Op
    = Inc
    | Dec


type CompOp
    = LT
    | LTE
    | EQ
    | NEQ
    | GT
    | GTE


applyInst : Inst -> ( Dict String Int, Int ) -> ( Dict String Int, Int )
applyInst { reg, op, amt, compReg, compOp, compVal } ( registers, max ) =
    let
        curVal =
            registers
                |> Dict.get reg
                |> Maybe.withDefault 0
                |> Debug.log "curVal"

        curCompVal =
            registers
                |> Dict.get compReg
                |> Maybe.withDefault 0
                |> Debug.log "curCompVal"

        passesComp =
            case compOp of
                LT ->
                    curCompVal < compVal

                LTE ->
                    curCompVal <= compVal

                EQ ->
                    curCompVal == compVal

                NEQ ->
                    curCompVal /= compVal

                GT ->
                    curCompVal > compVal

                GTE ->
                    curCompVal >= compVal

        newVal =
            if passesComp then
                case op of
                    Inc ->
                        curVal + amt

                    Dec ->
                        curVal - amt
            else
                curVal
    in
    ( Dict.insert reg newVal registers, Basics.max newVal max )


instFromStr : String -> Inst
instFromStr str =
    { reg =
        str
            |> regexSubSafetyDance "^(\\S+)"
    , op =
        str
            |> regexSubSafetyDance "^\\S+ (\\S+)"
            |> (\val ->
                    if val == "inc" then
                        Inc
                    else
                        Dec
               )
    , amt =
        regexSubSafetyDance "^\\S+ \\S+ (\\S+)" str
            |> String.toInt
            |> resultSafetyDance
    , compReg = regexSubSafetyDance "^\\S+ \\S+ \\S+ if (\\S+)" str
    , compOp =
        regexSubSafetyDance "^\\S+ \\S+ \\S+ if \\S+ (\\S+)" str
            |> (\val ->
                    case val of
                        "<" ->
                            LT

                        "<=" ->
                            LTE

                        "==" ->
                            EQ

                        "!=" ->
                            NEQ

                        ">" ->
                            GT

                        ">=" ->
                            GTE

                        _ ->
                            Debug.crash ("Bad compOp: " ++ val)
               )
    , compVal =
        regexSubSafetyDance "^\\S+ \\S+ \\S+ if \\S+ \\S+ (\\S+)" str
            |> String.toInt
            |> resultSafetyDance
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


regexSubSafetyDance : String -> String -> String
regexSubSafetyDance regex str =
    Regex.find (Regex.AtMost 1) (Regex.regex regex) str
        |> List.head
        |> maybeSafetyDance
        |> .submatches
        |> List.head
        |> maybeSafetyDance
        |> maybeSafetyDance
