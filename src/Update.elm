module Update exposing (update)

import Editor
import Model exposing (..)
import Messages exposing (..)
import Pendulum exposing (Pendulum(..), updatePendulum)
import Utils exposing (init, applyIndex)
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
            let
                debug = Debug.log "pendulums" model.pendulums
            in
                ( { model | pendulums = List.map (updatePendulum model.time (10 / 100)) model.pendulums
                  , time = model.time + (10 / 100)
                  }
                , Cmd.none
                )

        Update name index doubleIndex val ->
            ( { model | pendulums =
                          let
                              updatePend i p =
                                  if i == index then
                                      case p of
                                          Single _ ->
                                              Maybe.withDefault
                                                  p
                                                  ( String.toFloat val
                                                  |> Maybe.andThen (Pendulum.updateVal name Nothing p)
                                                  )
                                          Double _ _ ->
                                              Maybe.withDefault
                                                  p
                                                  ( String.toFloat val
                                                  |> Maybe.andThen (Pendulum.updateVal name doubleIndex p)
                                                  )
                                  else
                                      p
                          in
                              List.indexedMap updatePend model.pendulums
                      , editors = List.indexedMap (\i e -> if i == index
                                                           then Editor.updateEditor name val e
                                                           else e) model.editors
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
                      , editors = model.editors ++ (Pendulum.editorPendulum Pendulum.initPendulum)
              }
            , Cmd.none
            )

        RemovePendulum ->
            ( { model | pendulums = init model.pendulums
                      , editors = init model.editors }
            , Cmd.none
            )

        ToggleDouble index ->
            let
                pendulums = applyIndex Pendulum.toggleDouble index model.pendulums
                editors = List.concatMap Pendulum.editorPendulum pendulums
            in
                ( { model | pendulums = pendulums
                          , editors = editors
                  }
                , Cmd.none
                )
