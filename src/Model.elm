module Model exposing (..)

import Point exposing (Point(..))

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

type alias Model =
    { pageNumber : Int
    , pendulum : Pendulum
    , time : Float
    }

initPendulum : Pendulum
initPendulum =
    { pivotLocation = Point 100 100
    , theta = pi / 2
    , thetadot = 0
    , thetadotdot = 0
    , len = 200
    , damping = 0
    , driving = 0
    , drivingFreq = 0
    }


initModel : Model
initModel =
    { pageNumber = 1
    , pendulum = initPendulum
    , time = 0
    }
