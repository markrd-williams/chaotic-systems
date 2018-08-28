module Pendulum exposing (Pendulum)

import Point exposing (Point(..))
import Svg exposing (..)
import Svg.Attributes exposing (..)

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

renderPendulum : Pendulum -> Svg Msg
renderPendulum pendulum =
    let
        
