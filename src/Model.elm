module Model exposing (..)

import Editor exposing (Editor)
import Pendulum exposing (Pendulum, initPendulum)

type alias Model =
    { pageNumber : Int
    , pendulums : List Pendulum
    , time : Float
    , editors : List Editor
    , paused : Bool
    }

initModel : Model
initModel =
    { pageNumber = 1
    , pendulums = [initPendulum]
    , time = 0
    , editors = Pendulum.editorPendulum initPendulum
    , paused = False
    }
