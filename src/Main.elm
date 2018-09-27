import Browser
import Browser.Events exposing (onAnimationFrameDelta)
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
subscriptions {paused} =
    if paused then
        Sub.none
    else
        onAnimationFrameDelta Tick
