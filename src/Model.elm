module Model exposing (..)

import Editor exposing (Editor)
import Point exposing (Point(..))
import Html exposing (..)
import Messages exposing (..)
import Pendulum exposing (Pendulum, initPendulum)

type alias Model =
    { pageNumber : Int
    , pendulums : List Pendulum
    , time : Float
    , editor : List Editor
    , paused : Bool
    }

initModel : Model
initModel =
    { pageNumber = 1
    , pendulums = [initPendulum]
    , time = 0
    , editor = [Pendulum.editor initPendulum]
    , paused = False
    }
