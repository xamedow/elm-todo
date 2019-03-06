module Main exposing (Model, Msg(..), initialModel, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (keyCode, on, onClick, onDoubleClick, onInput)
import Json.Decode as Json



-- MODEL


type alias Todo =
    { id : Int
    , isEditable : Bool
    , status : Bool
    , title : String
    }


type alias Model =
    { newTodoTitle : Maybe String
    , todos : List Todo
    , changedTodoValue : String
    }


initialModel : Model
initialModel =
    { newTodoTitle = Nothing
    , todos = []
    , changedTodoValue = ""
    }

onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)

-- UPDATE


type Msg
    = NewTitleChange String
    | AddTodo
    | ChangeTodoStatus Int Bool
    | ChangeEditStatus Int Bool
    | ChangeTodoValue String
    | DeleteTodo Int
    | SubmitTodoValue Int Int

update : Msg -> Model -> Model
update msg model =


    case msg of
        NewTitleChange value ->
            if (String.length value) > 0 then
                    { model | newTodoTitle = Just value }
                else
                    { model | newTodoTitle = Nothing }

        AddTodo ->
            case model.newTodoTitle of
                Nothing ->
                    model
                Just title ->
                    { model | newTodoTitle = Nothing , todos = { isEditable = False, status = False, title = title, id = List.length model.todos + 1 } :: model.todos }

        DeleteTodo id ->
            let
                deleteTodo todo =
                    not (todo.id == id)
            in
            { model | todos = List.filter deleteTodo model.todos }

        ChangeTodoStatus id status ->
            let
                updateTodo todo =
                    if todo.id == id then
                        { todo | status = not status }
                    else
                        todo
            in
            { model | todos = List.map updateTodo model.todos }

        ChangeEditStatus id isEditable ->
            let
                updateTodo todo =
                    if todo.id == id then
                        { todo | isEditable = not isEditable }
                    else
                        todo
            in
            { model | todos = List.map updateTodo model.todos }

        ChangeTodoValue value ->
            { model | changedTodoValue = value }

        SubmitTodoValue key id ->
            if key == 13 then
                let
                    updateTodo todo title =
                        if todo.id == id then
                            { todo | title = title, isEditable = False }
                        else
                            todo
                in
                { model | todos = List.map (\ todo -> updateTodo todo model.changedTodoValue) model.todos }

            else if key == 27 then
                let
                    updateTodo todo =
                        if todo.id == id then
                            { todo | isEditable = False }
                        else
                            todo
                in
                { model | todos = List.map updateTodo model.todos, changedTodoValue = "" }
            else
                model



-- VIEW
viewTodoLabel : Todo -> Model -> Html Msg
viewTodoLabel todo model =
    if todo.status then
        s [] [ text todo.title ]
    else if todo.isEditable then
        input [ placeholder todo.title, onKeyDown (\ key -> SubmitTodoValue key todo.id) , onInput ChangeTodoValue, value model.changedTodoValue ] []
    else
        span [] [ text todo.title ]




viewTodo : Todo -> Model -> Html Msg
viewTodo todo model =
    li [] [ label [ onDoubleClick (ChangeEditStatus todo.id todo.isEditable) ] [ viewTodoLabel todo model ]
    , input [ type_ "checkbox", checked todo.status, onClick (ChangeTodoStatus todo.id todo.status) ] []
    , button [ onClick (DeleteTodo todo.id) ] [ text "X" ]
     ]



viewTodos : Model -> Html Msg
viewTodos model =
    case model.todos of
        [] -> div [] [ text "No todos found, so go ahead and just add one!"]
        _ -> ul [] (List.map (\ todo -> viewTodo todo model ) model.todos)



isNewTitleEmpty : Model -> Bool
isNewTitleEmpty model =
    case model.newTodoTitle of
        Nothing ->
            True
        Just _ ->
            False

getValue : Maybe String -> String
getValue value =
    case value of
        Nothing ->
            ""
        Just a ->
            a

viewAddTodo : Model -> Html Msg
viewAddTodo model =
    section [] [ label [ for "new-todo-input" ] [ text "Enter todo title" ]
        , input [ id "new-todo-input", type_ "text", value (getValue model.newTodoTitle), onInput NewTitleChange ] []
        , button [ disabled (isNewTitleEmpty model), onClick AddTodo ] [ text "Add todo" ]
        ]

view : Model -> Html Msg
view model =
    section []
        [ h1 [] [ text "What needs to be done" ]
        , viewAddTodo model
        , viewTodos model
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
