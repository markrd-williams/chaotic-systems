import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Messages exposing (Msg(..))
import Model exposing (Model, initModel)
import Update exposing (update)
import View exposing (view)

-- MAIN

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Cmd.none
    )

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.none
