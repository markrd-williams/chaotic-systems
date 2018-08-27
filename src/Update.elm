module Update exposing (update)

import Model exposing (..)
import Messages exposing (..)
import View

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NextPage ->
            let
                pageNumber =
                    if model.pageNumber + 1 > View.pageMax then
                        model.pageNumber
                    else
                        model.pageNumber + 1
            in
                ( { model | pageNumber = pageNumber }
                , Cmd.none
                )

        PrevPage ->
            let
                pageNumber =
                    if model.pageNumber - 1 < 1 then
                        model.pageNumber
                    else
                        model.pageNumber - 1
            in
                ( { model | pageNumber = pageNumber }
                , Cmd.none
                )
