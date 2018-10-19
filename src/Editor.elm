module Editor exposing (..)

import Html exposing (..)
import Html.Attributes as H exposing (..)
import Html.Events exposing (..)
import Messages exposing (Msg(..))

type alias Editor = List { name: String
                         , val: String
                         , min: String
                         , max: String
                         , step: String
                         }

updateEditor : String -> String -> Editor -> Editor
updateEditor checkName newVal editor =
    case editor of
        [] -> []
        (({name} as editorCell)::xs) ->
            if name == checkName then
                {editorCell | val = newVal} :: xs else
                editorCell :: (updateEditor checkName newVal xs)

viewEditBlock : Int -> Editor -> Html Msg
viewEditBlock index e =
    let
        go : Editor -> List (Html Msg)
        go editor =
            case editor of
                [] -> []

                ({name, val, min, max, step}::xs) ->
                    let
                        editValue =
                            div []
                                [ text (name ++ "  ")
                                , input [ type_ "range"
                                        , H.min min
                                        , H.max max
                                        , H.step step
                                        , value val
                                        , onInput (Update name index)] []
                                , span [] [text val]
                                ]
                    in
                        editValue :: (go xs)
    in
        div [ style "display" "inline-block"
            , style "border-style" "solid"
            , style "padding" "10px"
            , style "width" "90%"
            ]
            (go e)
