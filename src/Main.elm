module Main exposing (..)

import RippleCarryAdder exposing (..)
import Html exposing (Html, text, p, br, div)


main : Program Never Model msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : a -> Sub msg
subscriptions model =
    Sub.none


view : a -> Html msg
view model =
    div []
        [ text "Hello world!"
        , br [] []
        , viewHelper2 "andGate" andGate 0 0
        , viewHelper2 "andGate" andGate 0 1
        , viewHelper2 "andGate" andGate 1 0
        , viewHelper2 "andGate" andGate 1 1
        , viewHelper2 "orGate" orGate 0 0
        , viewHelper2 "orGate" orGate 0 1
        , viewHelper2 "orGate" orGate 1 0
        , viewHelper2 "orGate" orGate 1 1
        , viewHelper2 "halfAdder" halfAdder 0 0
        , viewHelper2 "halfAdder" halfAdder 0 1
        , viewHelper2 "halfAdder" halfAdder 1 0
        , viewHelper2 "halfAdder" halfAdder 1 1
        , viewHelper3 "fullAdder" fullAdder 0 0 0
        , viewHelper3 "fullAdder" fullAdder 0 0 1
        , viewHelper3 "fullAdder" fullAdder 0 1 0
        , viewHelper3 "fullAdder" fullAdder 0 1 1
        , viewHelper3 "fullAdder" fullAdder 1 0 0
        , viewHelper3 "fullAdder" fullAdder 1 0 1
        , viewHelper3 "fullAdder" fullAdder 1 1 0
        , viewHelper3 "fullAdder" fullAdder 1 1 1

        -- , viewHelper3 "rippleCarryAdder" rippleCarryAdder ( 1, 1, 1, 0 ) ( 1, 0, 1, 1 ) 0
        -- , viewHelper3 "rippleCarryAdder" rippleCarryAdder ( 1, 1, 1, 0 ) ( 1, 0, 1, 1 ) 1
        , viewHelper3 "rippleCarryAdder" rippleCarryAdder 0 0 0
        , viewHelper3 "rippleCarryAdder" rippleCarryAdder 0 0 1
        , viewHelper3 "rippleCarryAdder" rippleCarryAdder 1001 1101 1
        , viewHelper3 "rippleCarryAdder" rippleCarryAdder 1111 1111 0
        , viewHelper3 "rippleCarryAdder" rippleCarryAdder 1111 1111 1
        ]


viewHelper2 : String -> (a -> b -> c) -> a -> b -> Html msg
viewHelper2 name fun arg1 arg2 =
    div []
        [ text <| name ++ " " ++ (toString arg1) ++ " " ++ (toString arg2) ++ " = " ++ (toString <| fun arg1 arg2)
        , br [] []
        ]


viewHelper3 : String -> (a -> b -> c -> d) -> a -> b -> c -> Html msg
viewHelper3 name fun arg1 arg2 arg3 =
    div []
        [ text <| name ++ " " ++ (toString arg1) ++ " " ++ (toString arg2) ++ " " ++ (toString arg3) ++ " = " ++ (toString <| fun arg1 arg2 arg3)
        , br [] []
        ]


update : a -> b -> ( b, Cmd msg )
update msg model =
    ( model, Cmd.none )


type alias Model =
    {}


model : Model
model =
    {}


type Msg
    = Noop


init : ( Model, Cmd msg )
init =
    ( model, Cmd.none )
