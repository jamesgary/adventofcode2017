module Main exposing (main)

import Dict exposing (Dict)
import Html exposing (Html, div, text)


num =
    277678


main : Html msg
main =
    num
        |> solve
        |> toString
        |> text


type alias Pt =
    ( Int, Int )


type Dir
    = Up
    | Down
    | Left
    | Right


type alias Grid =
    Dict Pt Int


solve : Int -> Int
solve num =
    solveHelper (Dict.singleton ( 0, 0 ) 1) ( 0, 0 ) num


solveHelper : Grid -> Pt -> Int -> Int
solveHelper grid curPt num =
    let
        nextCellPt =
            applyDir (crawlDir curPt) curPt

        sum =
            sumNeighbors nextCellPt grid
    in
    if sum > num then
        sum
    else
        solveHelper (Dict.insert nextCellPt sum grid) nextCellPt num


sumNeighbors : Pt -> Grid -> Int
sumNeighbors ( x, y ) grid =
    [ ( x + 1, y + 1 )
    , ( x + 1, y )
    , ( x + 1, y - 1 )
    , ( x, y + 1 )
    , ( x, y - 1 )
    , ( x - 1, y + 1 )
    , ( x - 1, y )
    , ( x - 1, y - 1 )
    ]
        |> List.filterMap (\pt -> Dict.get pt grid)
        |> List.sum


type NumType
    = Pos
    | Neg
    | Zero


numToType : Int -> NumType
numToType num =
    if num == 0 then
        Zero
    else if num > 0 then
        Pos
    else
        Neg


crawlDir : Pt -> Dir
crawlDir ( x, y ) =
    case ( numToType x, numToType y ) of
        ( Zero, Zero ) ->
            Right

        ( Zero, Pos ) ->
            Left

        ( Zero, Neg ) ->
            Right

        ( Pos, Zero ) ->
            Up

        ( Neg, Zero ) ->
            Down

        ( Pos, Pos ) ->
            if x > y then
                Up
            else
                Left

        ( Neg, Pos ) ->
            if abs x < y then
                Left
            else
                Down

        ( Neg, Neg ) ->
            if abs x > abs y then
                Down
            else
                Right

        ( Pos, Neg ) ->
            if x <= abs y then
                Right
            else
                Up


applyDir : Dir -> Pt -> Pt
applyDir dir ( x, y ) =
    case dir of
        Up ->
            ( x, y + 1 )

        Down ->
            ( x, y - 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )
