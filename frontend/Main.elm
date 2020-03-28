module Main exposing (main)

import Api exposing (Entry, Id, Status(..), Task)
import Browser exposing (Document)
import Browser.Dom
import Dict exposing (Dict)
import FontAwesome.Icon exposing (viewIcon)
import FontAwesome.Solid as Solid
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Http
import Json.Decode as Json
import Task


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { newTask : String
    , tasks : Dict Id EditableTask
    }


type alias EditableTask =
    { task : Task
    , editing : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { tasks = Dict.empty
      , newTask = ""
      }
    , Api.getTasks RecvAllTasks
    )


fromTask : Task -> EditableTask
fromTask task =
    { task = task
    , editing = Nothing
    }


replaceTask : Task -> EditableTask -> EditableTask
replaceTask task editable =
    { editable
        | task = task
    }


toggleTask : Task -> Task
toggleTask task =
    case task.status of
        Open ->
            { task | status = Done }

        Done ->
            { task | status = Open }



-- UPDATE


type Msg
    = NewTaskInput String
    | NewTaskCancel
    | NewTaskSubmit
    | RecvNewTask (HttpResult (Entry Task))
    | RecvAllTasks (HttpResult (List (Entry Task)))
    | TaskMsg Id TaskMsg
    | Ignore


type TaskMsg
    = Toggle
    | Edit
    | EditInput String
    | EditCancel
    | EditSubmit
    | Delete
    | RecvUpdate (HttpResult (Entry Task))
    | RecvDelete (HttpResult ())


type alias DomResult a =
    Result Browser.Dom.Error a


type alias HttpResult a =
    Result Http.Error a


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        logError tag error =
            logErrorIn tag error ( model, Cmd.none )
    in
    case msg of
        NewTaskInput newTask ->
            ( { model | newTask = newTask }, Cmd.none )

        NewTaskCancel ->
            ( { model | newTask = "" }, Cmd.none )

        NewTaskSubmit ->
            if String.isEmpty model.newTask then
                ( model, Cmd.none )

            else
                let
                    postTask =
                        { label = model.newTask
                        , status = Open
                        }
                in
                ( { model | newTask = "" }
                , Api.postTasks postTask RecvNewTask
                )

        RecvNewTask (Err error) ->
            logError "Failed to receive new task" error

        RecvNewTask (Ok entry) ->
            let
                newTasks =
                    Dict.insert entry.id (fromTask entry.value) model.tasks
            in
            ( { model | tasks = newTasks }, Cmd.none )

        RecvAllTasks (Err error) ->
            logError "Failed to receive task list" error

        RecvAllTasks (Ok list) ->
            let
                updateEditable : Entry Task -> Maybe EditableTask -> Maybe EditableTask
                updateEditable entry mbTask =
                    case mbTask of
                        Nothing ->
                            Just (fromTask entry.value)

                        Just task ->
                            Just (replaceTask entry.value task)

                insertEntry : Entry Task -> Dict Id EditableTask -> Dict Id EditableTask
                insertEntry entry =
                    Dict.update entry.id (updateEditable entry)

                newTasks =
                    List.foldl insertEntry model.tasks list
            in
            ( { model | tasks = newTasks }, Cmd.none )

        TaskMsg id taskMsg ->
            case Dict.get id model.tasks of
                Nothing ->
                    logError "Message for missing task" ( id, taskMsg )

                Just task ->
                    let
                        ( mbNewTask, cmd ) =
                            updateEditableTask id taskMsg task

                        newTasks =
                            case mbNewTask of
                                Nothing ->
                                    Dict.remove id model.tasks

                                Just newTask ->
                                    Dict.insert id newTask model.tasks
                    in
                    ( { model | tasks = newTasks }, cmd )

        Ignore ->
            ( model, Cmd.none )


updateEditableTask : Id -> TaskMsg -> EditableTask -> ( Maybe EditableTask, Cmd Msg )
updateEditableTask id msg editable =
    let
        logError tag error =
            logErrorIn tag error ( Just editable, Cmd.none )

        toMsg =
            TaskMsg id
    in
    case msg of
        Toggle ->
            ( Just editable
            , Api.putTasksById id (toggleTask editable.task) (RecvUpdate >> toMsg)
            )

        Edit ->
            ( Just { editable | editing = Just editable.task.label }
            , Task.attempt (\_ -> Ignore) (Browser.Dom.focus (taskDomId id))
            )

        EditInput input ->
            ( Just { editable | editing = Just input }
            , Cmd.none
            )

        EditCancel ->
            ( Just { editable | editing = Nothing }
            , Cmd.none
            )

        EditSubmit ->
            case editable.editing of
                Nothing ->
                    logError "Not editing" id

                Just value ->
                    ( Just { editable | editing = Nothing }
                    , if String.isEmpty value then
                        Api.deleteTasksById id (RecvDelete >> toMsg)

                      else
                        let
                            oldTask =
                                editable.task

                            newTask =
                                { oldTask | label = value }
                        in
                        Api.putTasksById id newTask (RecvUpdate >> toMsg)
                    )

        Delete ->
            ( Just editable
            , Api.deleteTasksById id (RecvDelete >> toMsg)
            )

        RecvUpdate (Err error) ->
            logError "Failed to receive task update" ( id, error )

        RecvUpdate (Ok entry) ->
            if entry.id /= id then
                logError "Received task update for the wrong id" ( entry.id, id )

            else
                ( Just { editable | task = entry.value }
                , Cmd.none
                )

        RecvDelete (Err error) ->
            logError "Failed to delete task" ( id, error )

        RecvDelete (Ok ()) ->
            ( Nothing
            , Cmd.none
            )


logErrorIn : String -> a -> b -> b
logErrorIn tag error value =
    case Debug.log tag error of
        _ ->
            value



-- VIEW


view : Model -> Document Msg
view model =
    { title = "todo app"
    , body =
        [ div
            [ class "container" ]
            [ div
                [ classList_ [ "row", "justify-content-center" ] ]
                [ div
                    [ classList_ [ "col-12", "col-sm-10", "col-md-8", "col-lg-6", "col-xl-4" ] ]
                    [ div
                        [ class "text-center" ]
                        [ h1 [] [ text "todo app" ] ]
                    , viewNewTaskInput model.newTask
                    , Keyed.ul
                        [ class "list-group" ]
                        (List.map viewEditableTask (Dict.toList model.tasks))
                    ]
                ]
            ]
        ]
    }


viewNewTaskInput : String -> Html Msg
viewNewTaskInput newTask =
    div
        [ class "input-group" ]
        [ input
            [ class "form-control"
            , placeholder "new task"
            , value newTask
            , onInput NewTaskInput
            , let
                handler code =
                    case code of
                        13 ->
                            NewTaskSubmit

                        27 ->
                            NewTaskCancel

                        _ ->
                            Ignore
              in
              onKeyDown handler
            ]
            []
        , div
            [ class "input-group-append" ]
            [ button
                [ classList_ [ "btn", "btn-outline-primary" ]
                , onClick NewTaskSubmit
                ]
                [ viewIcon Solid.paperPlane ]
            , button
                [ classList_ [ "btn", "btn-outline-secondary" ]
                , onClick NewTaskCancel
                ]
                [ viewIcon Solid.backspace ]
            ]
        ]


viewEditableTask : ( Id, EditableTask ) -> ( String, Html Msg )
viewEditableTask ( id, { editing, task } ) =
    let
        toMsg =
            TaskMsg id

        viewTask =
            [ button
                [ classList_ [ "form-control", "from-control-sm" ]
                , onClick (toMsg Toggle)
                ]
                [ if task.status == Done then
                    del [] [ text task.label ]

                  else
                    text task.label
                ]
            , div
                [ class "input-group-append" ]
                [ button
                    [ classList_ [ "btn", "btn-outline-secondary" ]
                    , onClick (toMsg Edit)
                    ]
                    [ viewIcon Solid.edit ]
                , button
                    [ classList_ [ "btn", "btn-outline-secondary" ]
                    , onClick (toMsg Delete)
                    ]
                    [ viewIcon Solid.trash ]
                ]
            ]

        viewEdit edit =
            [ input
                [ Html.Attributes.id (taskDomId id)
                , classList_ [ "form-control", "form-control-sm" ]
                , placeholder "task decription"
                , value edit
                , onInput (EditInput >> toMsg)
                , let
                    handler code =
                        case code of
                            -- enter
                            13 ->
                                toMsg EditSubmit

                            -- escape
                            27 ->
                                toMsg EditCancel

                            _ ->
                                Ignore
                  in
                  onKeyDown handler
                , onBlur (toMsg EditSubmit)
                ]
                []
            ]
    in
    ( String.fromInt id
    , li
        [ class "list-group-item" ]
        [ div
            [ classList_ [ "input-group", "input-group-sm" ] ]
            (case editing of
                Nothing ->
                    viewTask

                Just edit ->
                    viewEdit edit
            )
        ]
    )


taskDomId : Id -> String
taskDomId id =
    "task-" ++ String.fromInt id


classList_ : List String -> Attribute msg
classList_ =
    List.map (\attr -> ( attr, True )) >> classList


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
