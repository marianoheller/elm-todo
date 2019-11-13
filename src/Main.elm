port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json



-- MODEL


type alias TodoTask =
    { name : String
    , done : Bool
    , id : Int
    , deleted : Bool
    }


type alias Model =
    { tasks : List TodoTask
    , input : String
    }


findLastId : List TodoTask -> Int
findLastId =
    List.foldr (\a acc -> Basics.max acc a.id) 0


generateId : List TodoTask -> Int
generateId tasks =
    findLastId tasks + 1


newTask : String -> List TodoTask -> TodoTask
newTask str tasks =
    let
        newId =
            generateId tasks
    in
    { name = str, done = False, id = newId, deleted = False }


init : Maybe Model -> ( Model, Cmd Msg )
init maybeSavedModel =
    case maybeSavedModel of
        Just savedModel ->
            ( savedModel, Cmd.none )

        Nothing ->
            ( { tasks = []
              , input = ""
              }
            , Cmd.none
            )



---- UPDATE ----


type Msg
    = InputChanged String
    | EnterPressed
    | DeleteClicked Int
    | DoneClicked Int
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputChanged val ->
            ( { model | input = val }, Cmd.none )

        EnterPressed ->
            if model.input == "" then
                ( model, Cmd.none )

            else
                let
                    newModel =
                        { model | input = "", tasks = newTask model.input model.tasks :: model.tasks }
                in
                ( newModel, cache newModel )

        DeleteClicked id ->
            let
                -- TODO: improve so it only saves last 10 deletes or something
                taskDeleter task =
                    if task.id == id then
                        { task | deleted = True }

                    else
                        task

                tasks =
                    List.map taskDeleter model.tasks

                newModel =
                    { model | tasks = tasks }
            in
            ( newModel, cache newModel )

        DoneClicked id ->
            let
                taskCompleter task =
                    if task.id == id then
                        { task | done = True }

                    else
                        task

                tasks =
                    List.map taskCompleter model.tasks

                newModel =
                    { model | tasks = tasks }
            in
            ( newModel, cache newModel )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


onEnter : Attribute Msg
onEnter =
    let
        getMsg kc =
            if kc == 13 then
                EnterPressed

            else
                NoOp
    in
    on "keydown" (Json.map getMsg keyCode)


view : Model -> Html Msg
view model =
    div [ class "todoapp" ]
        [ h1 [] [ text "todos" ]
        , viewInput model.input
        , viewTasks model.tasks
        ]


viewInput : String -> Html Msg
viewInput inputVal =
    input [ onInput InputChanged, onEnter, value inputVal ] []


viewTasks : List TodoTask -> Html Msg
viewTasks tasks =
    let
        filteredTasks =
            List.filter (\task -> not task.deleted) tasks

        getLiClass task =
            if task.done then
                class "done"

            else
                class ""
    in
    ul [ class "todo-list" ] (List.map (\task -> li [ getLiClass task ] (viewTask task)) filteredTasks)


viewTask : TodoTask -> List (Html Msg)
viewTask task =
    let
        deleted =
            if task.deleted then
                "deleted"

            else
                ""
    in
    [ viewDoneCheckbox task.id
    , label [ class deleted ] [ text task.name ]
    , viewDeleteButton task.id
    ]


viewDeleteButton : Int -> Html Msg
viewDeleteButton id =
    button [ class "delete", onClick (DeleteClicked id) ] []


viewDoneCheckbox : Int -> Html Msg
viewDoneCheckbox id =
    input [ type_ "checkbox", class "toggle", onClick (DoneClicked id) ] [ text "done" ]



---- PORTS ----


port cache : Model -> Cmd msg



---- PROGRAM ----


main : Program (Maybe Model) Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
