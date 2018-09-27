module Update exposing (update)

import Editor
import Model exposing (..)
import Messages exposing (..)
import Pendulum exposing (Pendulum, updatePendulum)
import Utils exposing (init)
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

        Tick time ->
            ( { model | pendulums = List.map (updatePendulum model.time (time / 100)) model.pendulums
                      , time = model.time + (time / 100)
              }
            , Cmd.none
            )

        Update name index val ->
            ( { model | pendulums =
                          let
                              updatePend i p =
                                  if i == index then
                                      let
                                          maybeVal = String.toFloat val
                                          maybePend =
                                              case maybeVal of
                                                  Just v -> Pendulum.updateVal name v p
                                                  Nothing -> Nothing
                                      in
                                          case maybePend of
                                              Just p2 -> p2
                                              Nothing -> p
                                  else
                                      p
                          in
                              List.indexedMap updatePend model.pendulums
                      , editor = List.indexedMap (\i e -> if i == index
                                                          then Editor.updateEditor name val e
                                                          else e) model.editor
              }
            , Cmd.none
            )

        Pause ->
            ( { model | paused = not model.paused}
            , Cmd.none
            )

        StepF ->
            ( { model | pendulums = List.map (updatePendulum model.time 1) model.pendulums
                      , time = model.time + 1
              }
            , Cmd.none
            )

        StepB ->
            ( { model | pendulums = List.map (updatePendulum model.time (-1)) model.pendulums
                      , time = model.time - 1
              }
            , Cmd.none
            )

        AddPendulum ->
            ( { model | pendulums = model.pendulums ++ [Pendulum.initPendulum]
                      , editor = model.editor ++ [Pendulum.editor Pendulum.initPendulum]
              }
            , Cmd.none
            )

        RemovePendulum ->
            ( { model | pendulums = init model.pendulums
                      , editor = init model.editor }
            , Cmd.none
            )
