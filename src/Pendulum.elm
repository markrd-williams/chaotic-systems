module Pendulum exposing (Pendulum, initPendulum, renderPendulum, updatePendulum, updateVal, editor)

import Editor exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Messages exposing (Msg(..))
import Point exposing (Point(..))
import Svg exposing (..)
import Svg.Attributes as S exposing (..)

type alias Pendulum =
    { pivotLocation : Point Int
    , theta : Float
    , thetadot : Float
    , thetadotdot : Float
    , len : Int
    , damping : Float
    , driving : Float
    , drivingFreq : Float
    }

initPendulum : Pendulum
initPendulum =
    { pivotLocation = Point 300 220
    , theta = pi / 2
    , thetadot = 0
    , thetadotdot = 0
    , len = 200
    , damping = 0
    , driving = 0
    , drivingFreq = 0
    }


calcThetadotdot : Pendulum -> Float -> Float
calcThetadotdot { theta, thetadot, thetadotdot
                , len, damping, driving, drivingFreq
                } time =
    let
        accelerationMass    = ((-9.8) / (toFloat len)) * sin theta
        accelerationDamping = damping * thetadot
        accelerationDriving = driving / (toFloat len) * cos (drivingFreq * time)
    in
        accelerationMass - accelerationDamping + accelerationDriving

updatePendulum : Float -> Float -> Pendulum -> Pendulum
updatePendulum time step pendulum =
    let
        thetadotdot =
            calcThetadotdot pendulum time

        thetadot =
            euler pendulum.thetadot thetadotdot step

        theta =
            euler pendulum.theta thetadot step

        thetaFix =
            if theta > pi then
                theta - 2 * pi
            else if theta <= -pi then
                2 * pi + theta
            else
                theta
    in
        { pendulum
          | theta = thetaFix
          , thetadot = thetadot , thetadotdot = thetadotdot}

euler : Float -> Float -> Float -> Float
euler y ydot step =
    y + ydot * step

renderPendulum :  Float -> Pendulum -> List (Svg Msg)
renderPendulum time ({theta, len, pivotLocation} as p) =
    let
        pendulumCoords = Point.add (Point.toInt (Point.fromPolar (toFloat len, theta))) pivotLocation
        pCoordsLog = Debug.log (Point.logPointI pendulumCoords)
    in
        [ circle
            [ cx (String.fromInt (Point.getX pendulumCoords))
            , cy (String.fromInt (Point.getY pendulumCoords))
            , r "20"
            , stroke "black"
            , fill "black"
            ] []
        , line
            [ x1 (String.fromInt (Point.getX pivotLocation))
            , y1 (String.fromInt (Point.getY pivotLocation))
            , x2 (String.fromInt (Point.getX pendulumCoords))
            , y2 (String.fromInt (Point.getY pendulumCoords))
            , S.style "stroke:rgb(0,0,0);stroke-width:2"
            ] []
        ] ++ (renderForceArrow p time)

renderForceArrow : Pendulum -> Float -> List (Svg Msg)
renderForceArrow {driving, drivingFreq} time =
    let
        scaledDriving =
            round (200 * cos (drivingFreq * time))

        p1 = Point 300 500

        p2 = Point (300 + scaledDriving) 500
    in
        if driving /= 0 then
            drawArrow p1 p2
        else
            []

drawArrow : Point Int -> Point Int -> List (Svg Msg)
drawArrow (Point x1 y1) (Point x2 y2) =
    let
        k =
            if x2 > x1 then
                1
            else
                -1
    in
        [ line
              [ S.x1 (String.fromInt x1)
              , S.y1 (String.fromInt y1)
              , S.x2 (String.fromInt x2)
              , S.y2 (String.fromInt y2)
              , S.style "stroke:rgb(0,0,0);stroke-width:2"
              ] []
        , line
              [ S.x1 (String.fromInt (x2 - k * 20))
              , S.y1 (String.fromInt (y2 + 20))
              , S.x2 (String.fromInt x2)
              , S.y2 (String.fromInt y2)
              , S.style "stroke:rgb(0,0,0);stroke-width:2"
              ] []
        , line
              [ S.x1 (String.fromInt (x2 - k * 20))
              , S.y1 (String.fromInt (y2 - 20))
              , S.x2 (String.fromInt x2)
              , S.y2 (String.fromInt y2)
              , S.style "stroke:rgb(0,0,0);stroke-width:2"
              ] []
        ]

editor : Pendulum -> Editor
editor pendulum =
    [ { name = "length"
      , val = String.fromInt pendulum.len
      , min = "50"
      , max = "200"
      , step = "1"
      }
    , { name = "damping"
      , val = String.fromFloat pendulum.damping
      , min = "0.0"
      , max = "0.5"
      , step = "0.01"
      }
    , { name = "drive force"
      , val = String.fromFloat pendulum.driving
      , min = "0.0"
      , max = "10.0"
      , step = "0.1"
      }
    , { name ="drive freq"
      , val = String.fromFloat pendulum.drivingFreq
      , min = "0.0"
      , max = "1.0"
      , step = "0.01"
      }
    ]

updateVal : String -> Float -> Pendulum -> (Maybe Pendulum)
updateVal name val pendulum =
    case name of
        "length" ->
            Just { pendulum | len = (round val) }
        "damping" ->
            Just { pendulum | damping = val }
        "drive force" ->
            Just { pendulum | driving = val }
        "drive freq" ->
            Just { pendulum | drivingFreq = val }
        _ ->
            Nothing
