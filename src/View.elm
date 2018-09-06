module View exposing (view, pageMax)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (Model)
import Messages exposing (Msg(..))
import Pendulum exposing (Pendulum, renderPendulum)
import Svg
import Svg.Attributes

import Point exposing (Point(..))

view : Model -> Html Msg
view model =
    div []
        [ viewExplanation model.pageNumber
        , viewSimulation model
        , viewFooter
        , div [] [ text (Point.logPointI (Point.add (Point 1 2) (Point 3 4))) ]
        ]

viewExplanation : Int -> Html Msg
viewExplanation pageNumber =
    let
        explanation = Maybe.withDefault "error" (getText pageNumber)
    in
        div []
            [ span [] [ button [ onClick PrevPage ] [ text "<|" ] ]
            , span [ style "border-style" "solid"
                   , style "border-width" "1px"
                   , style "padding" "5px"
                   , style "display" "inline-block"
                   ]
                   [ text explanation ]
            , span [] [ button [ onClick NextPage ] [ text "|>" ] ]
            ]

viewSimulation : Model -> Svg.Svg Msg
viewSimulation model =
    Svg.svg
        [ Svg.Attributes.width "600"
        , Svg.Attributes.height "600"
        , Svg.Attributes.viewBox "0 0 600 600"
        ]
        (renderPendulum model.pendulum)

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

