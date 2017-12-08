module Main exposing (main)

import Html exposing (Html, div, text)


num =
    277678


cases =
    [ ( 1, ( 0, 0 ) )
    , ( 2, ( 1, 0 ) )
    , ( 3, ( 1, 1 ) )
    , ( 4, ( 0, 1 ) )
    , ( 5, ( -1, 1 ) )
    , ( 6, ( -1, 0 ) )
    , ( 7, ( -1, -1 ) )
    , ( 8, ( 0, -1 ) )
    , ( 9, ( 1, -1 ) )
    , ( 10, ( 2, -1 ) )
    , ( 11, ( 2, 0 ) )
    , ( 12, ( 2, 1 ) )
    , ( 13, ( 2, 2 ) )
    , ( 14, ( 1, 2 ) )
    , ( 25, ( 2, -2 ) )
    ]


answer =
    ptFromNum num
        |> (\( x, y ) -> abs x + abs y)


main : Html msg
main =
    div []
        [ div []
            (cases
                |> List.map
                    (\( num, expected ) ->
                        div []
                            [ text (toString num ++ ": " ++ toString (ptFromNum num) ++ " (expected " ++ toString expected ++ ")")
                            ]
                    )
            )
        , div [] [ text ("Answer: " ++ toString answer) ]
        ]


type alias Pt =
    ( Int, Int )


ptFromNum : Int -> Pt
ptFromNum num =
    step ( 0, 0 ) (num - 1)


step : Pt -> Int -> Pt
step (( x, y ) as pt) steps =
    case steps of
        0 ->
            pt

        _ ->
            step (applyDir (crawlDir pt) pt) (steps - 1)



-- let's use math coordinates, not canvas coordinates


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


bad =
    ( -999, -999 )


type Dir
    = Up
    | Down
    | Left
    | Right


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
