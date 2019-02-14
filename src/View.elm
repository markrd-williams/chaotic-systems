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
                       , style "margin" "1%"
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
        explanation = Maybe.withDefault [text "error"] (getText pageNumber)
    in
        div [ style "width" "25%"
            , style "height" "580px"
            , style "float" "left"
            ]
            [ span [ style "border-style" "solid"
                   , style "border-width" "1px"
                   , style "padding" "5px"
                   , style "display" "inline-block"
                   ]
                   explanation
            , span [] [ button [ onClick PrevPage ] [ text "<|" ] ]
            , span [ style "float" "right" ]
                   [ button [ onClick NextPage ] [ text "|>" ] ]
            ]

viewEditor : List Editor -> Html Msg
viewEditor editors =
    div [ style "overflow-y" "scroll"
        , style "width" "24%"
        , style "height" "540px"
        , style "margin" "1% 0"
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
    footer [ style "position" "fixed"
           , style "bottom" "50px"]
           [ text "An introduciton to chaotic systems, written in elm "
           , a [href "https://github.com/markrd-williams/chaotic-systems"]
               [text "Source code"]
           ]

eqn : String -> Html Msg
eqn str =
    p [style "text-align" "center"] [em [] [text str]]


pageMax : Int
pageMax = 6

getText : Int -> Maybe (List (Html Msg))
getText pageNum =
    case pageNum of
        1 ->
            Just [ h1 [] [text "Chaotic Systems"]
                 , p [] [text "This is an introduction to chaotic systems. Lots of people have heard the phrases \"chaos theory\" and \"the butterfly effect\", but what actually is it? Chaos theory is a study of seemingly random systems, named chaotic systems. Chaos theory states that within these random systems there are patterns and rules that appear, such as feedback loops and repetition. Chaotic systems are also very sensitive to initial conditions, meaning that the initial settings for them can cause big differences for the end result."]
                 , p [] [text "This simulation is intended to give an intuition for how these systems work by editing and playing with 2 different chaotic systems. Click the next button to get started with the simulation."]
                 ]

        2 ->
            Just [ text "Before we look at chaotic systems first observe the single pendulum on the screen. This is currently not a chaotic system; the movement of the pendulum is predictable and not at all random. This is because currently the simulation does not have any extra bits which would make it chaotic. Let's look at how a simple pendulum, such as this is modeled. To model a pendulum like this we use "
                 , em [] [text "differential equations"]
                 , text". Despite the long name, what we need to understand isn't that difficult. For our purposes, a differential equation is a way of linking the position of the pendulum at any point, with the acceleration acting on it. This means given a position we can calculate the acceleration and therefore simulate the movement."
                 , p [] [text "The single pendulum equation looks like this:"]
                 , eqn "a = A sin(θ)"
                 , p [] [text "Where ", em [] [text "A"], text " is an arbitrary constant, ", em [] [text "θ"], text " is the angle the pendulum makes with the normal and ", em [] [text "a"], text " is the acceleration."]
                 ]

        3 ->
            Just [ text "We can make this simple model a bit more complex by adding in a damping force. This is the effect of air resistance on the pendulum. As air resistance is proportional to velocity we can model it by subtracting a multiple of the velocity from the acceleration at a given point. Try dragging the \"damping\" slider to a value higher than 0. See how the value effects the motion of the pendulum."
                 , p [] [text "The equation of motion now looks like:"]
                 , eqn "a = A sin(θ) - Bv"
                 , p [] [text "Where ", em [] [text "B"], text " is an arbitrary constant, ", em [] [text "v"], text " is velocity and all other symbols are the same as previously."]
                 , p [] [text "The motion at this point is however still very predictable and not at all chaotic. To begin with the chaotic system, turn over the page."]
                 ]
        4 ->
            Just [ text "Now we add another piece of complexity to the model. We will do this by adding a driving force. This force will oscillate back and forth regularly with time. To add this to the differential equation, we add a \"forcing function\" which is a force independent of position. The new equaiton is:"
                 , eqn "a = A sin(θ) - Bv + C cos(kt)"
                 , p [] [text "Where ", em [] [text "C"], text " and ", em [] [text "k"], text " are both arbitrary constants and ", em [] [text "t"], text " is time."]
                 , p [] [text "The forcing function is the new part of the equation. This can be used as our forcing function because ", em [] [text "cos"], text " is a function that oscillates up and down between a minimum and a maximum. The arbitrary constants allow us to change the amplitude, or maximum force applied and the frequency of the oscillations." ]
                 , p [] [text "This final piece of complexity makes the model chaotic. It can be finicky so play with the values till you get motion that is irregular and unpredictable. Use the the add pendulum button to add two pendulums, and compare the way they move. " ]
                 ]

        5 ->
            Just [ em [] [text "Disclaimer: In the current version of the simulation, the double pendulum does not work as expected, causing it to increase in speed over time. Hopefully the bug will be fixed soon."]
                 , p [] [text "Now try changing a pendulum from single to double, by pressing the \"Toggle single/double\" button. Observe how the motion is immediately complicated despite the lack of any driving or damping force. The additional weight of the second pendulum acts on the first pendulum, and the weight of the first pendulum acts on the second. This interaction between the two bobs creates the chaotic motion."]
                 , p [] [text "The equations for the double pendulum are a lot more complicated to work with, so I won't put them here, however ", a [href "http://scienceworld.wolfram.com/physics/DoublePendulum.html"] [text "Wolfram ScienceWorld"], text " offers a fantastic set of explanations on how to derive them and how to understand them."]
                 ]

        6 ->
            Just [text "Now that we've seen all the options for pendulums, try experimenting with the other features: Add more pendulums, pause and sync them, compare their motions; try messing with the sliders to create interesting motions; step forwards and backwards to watch the simulation slowly. "
                 ]

        _ ->
            Nothing

