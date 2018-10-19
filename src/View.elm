module View exposing (view, pageMax)

import Editor exposing (Editor, viewEditBlock)
import Html exposing (..)
import Html.Attributes as H exposing (..)
import Html.Events exposing (..)
import Model exposing (Model)
import Messages exposing (Msg(..))
import Pendulum exposing (Pendulum, renderPendulum)
import Svg
import Svg.Attributes as S

import Point exposing (Point(..))

view : Model -> Html Msg
view model =
    div []
        [ div [] [ div [ style "display" "block"
                       , style "float" "left"
                       , style "position" "relative"
                       , style "border-style" "solid"
                       , style "margin" "10px"
                       ]
                       [viewSimulation model]
                 , (viewMenu model.paused)
                 , viewEditor model.editors
                 , viewExplanation model.pageNumber
                 ]
        , viewFooter
        ]

viewExplanation : Int -> Html Msg
viewExplanation pageNumber =
    let
        explanation = Maybe.withDefault "error" (getText pageNumber)
    in
        div [ style "width" "25%"
            , style "height" "560px"
            , style "float" "right"
            ]
            [ span [ style "border-style" "solid"
                   , style "border-width" "1px"
                   , style "padding" "5px"
                   , style "display" "inline-block"
                   ]
                   [ text explanation ]
            , span [] [ button [ onClick PrevPage ] [ text "<|" ] ]
            , span [ style "float" "right" ]
                   [ button [ onClick NextPage ] [ text "|>" ] ]
            ]

viewEditor : List Editor -> Html Msg
viewEditor editors =
    div [ style "overflow-y" "scroll"
        , style "width" "25%"
        , style "height" "540px"
        , style "margin" "7px 0"
        , style "display" "block"
        , style "position" "relative"
        , style "float" "left"
        ]
        [ div []
              [ button [onClick AddPendulum] [text "add pendulum"]
              , button [onClick RemovePendulum] [text "remove"]
              ]
        , div []
            (List.indexedMap viewEditBlock editors)
        ]

viewSimulation : Model -> Svg.Svg Msg
viewSimulation {pendulums, time} =
    Svg.svg
        [ S.width "600"
        , S.height "550"
        , S.viewBox "0 0 600 550"
        ]
        (List.concatMap (renderPendulum time) pendulums)

viewMenu : Bool -> Html Msg
viewMenu paused =
    let
        pauseText =
            if paused then
                "play"
            else
                "pause"
    in
    div []
        [ button [ onClick Pause ] [ text pauseText ]
        , button [ onClick StepF
                 , disabled (not paused)
                 ]
                 [ text "step" ]
        , button [ onClick StepB
                 , disabled (not paused)
                 ]
                 [ text "step back" ]
        ]

viewFooter : Html Msg
viewFooter =
    footer []
           [ text "An introduciton to chaotic systems, written in elm "
           , a [href "https://github.com/markrd-williams/chaotic-systems"]
               [text "Source code"]
           ]

pageMax : Int
pageMax = 3

getText : Int -> Maybe String
getText pageNum =
    case pageNum of
        1 ->
            Just "This is an introduction to Chaotic Systems. A chaotic system is very sensitive to initial conditions."

        2 ->
            Just "Here is a very simple model: A single pendulum. This is not chaotic. Changing the starting angle or the length of the does not cause wildly different results,"

        3 ->
            Just "Now we add a driving force to the pendulum. Notice now that the motion is more erratic and less predictable."

        _ ->
            Nothing

